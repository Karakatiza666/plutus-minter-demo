{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.Withdraw where

import Cardano.Api.Shelley (PlutusScriptV1)
import Cardano.Plutus.FromBS
import Cardano.Plutus.Common
import Cardano.Plutus.Validator
import Cardano.Plutus.V1.Contexts
import Cardano.Plutus.OnChain.Bridge.WithdrawFactory

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString qualified as Strict
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)


import Ledger.Ada qualified as Ada
import Ledger.Address
import Ledger.Credential (Credential(..))
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain ()
import Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInfo (..), TxInInfo(..), ownCurrencySymbol, valuePaidTo, valueProduced,
                        getContinuingOutputs, scriptOutputsAt, findOwnInput, ownHash)
import Plutus.V1.Ledger.Contexts qualified as Validation
import Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Api 
import Ledger.TimeSlot qualified as TimeSlot
-- import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test hiding (not)

import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold)
import PlutusTx.Prelude qualified as PP
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins
import PlutusTx.Foldable (foldl)
import PlutusTx.Maybe (maybe)
import Prelude as Haskell (Monoid(..), Semigroup (..), show, fromIntegral, toInteger, undefined, error)
import Prelude qualified as Haskell
import Schema (ToSchema)
import Unsafe.Coerce
import Control.Arrow ((&&&))

newtype WithdrawRedeemer = WithdrawRedeemer {
    newOwner :: ValidatorHash
} deriving stock Generic
PlutusTx.unstableMakeIsData ''WithdrawRedeemer
PlutusTx.makeLift ''WithdrawRedeemer

newtype WithdrawDatum = WithdrawDatum {
    deadline :: POSIXTime
}
    deriving stock Generic
    deriving anyclass Eq
PlutusTx.unstableMakeIsData ''WithdrawDatum

data WithdrawValidator
instance Scripts.ValidatorTypes WithdrawValidator where
   type instance DatumType WithdrawValidator = WithdrawDatum
   type instance RedeemerType WithdrawValidator = Maybe WithdrawRedeemer

-- Allows owner to deposit CNTs (excl. ADA) from Hold UTxO to Withdraw UTxO, existing one or newly created from Factory
-- Withdraw policy is parameterized by client PubKeyHash, who is allowed to claim CNTs in it
-- Client PubKeyHash is stored as a policy parameter to simplify discovery of withdrawals client is eligible for
-- Client of Withdraw UTxO can only withdraw all funds from it at once except ADA change, which is returned to the Factory
-- After UTxO time lock, specified in Datum, expires - it's CNTs (excl. ADA) can be returned to the Hold UTxO (by anyone), with change returned to the Factory
mkWithdrawValidator :: ValidatorHash -> PubKeyHash -> PubKeyHash -> WithdrawDatum -> Maybe WithdrawRedeemer -> ScriptContext -> Bool
mkWithdrawValidator _factoryScriptHash owner client datum redeemer ctx@ScriptContext{scriptContextTxInfo=txIn} =
    maybe (factoryValidated && (signedByClient || deadlineReached && signedByOwner)) ((signedByOwner &&) . ownerTransferOk) redeemer
    where
        factoryValidated = length (scriptTxInsThat txIn (_factoryScriptHash ==)) > 0
        deadlineReached = Interval.contains (Interval.from $ deadline datum) $ txInfoValidRange txIn
        signedByClient = Validation.txSignedBy txIn client
        signedByOwner = Validation.txSignedBy txIn owner
        ownerTransferOk WithdrawRedeemer {..} =
            length (getContinuingOutputs ctx) == 0
            -- Tx only has one script output - for new owner hash, with same datum and value as current one
         && hasOnlyScriptOut txIn (or . flap (utxoValid . txOutValue . txInInfoResolved <$> findOwnInput ctx))
         && onlyPaidTo txIn owner -- to avoid accidentally sending money to client and forgetting to update nonce
            where
                utxoValid ownValue (h, v, d) = h == newOwner && v == ownValue && d == datum

withdrawScript :: PubKeyHash -> PubKeyHash -> Scripts.TypedValidator WithdrawValidator
withdrawScript owner client = Scripts.mkTypedValidator @WithdrawValidator
    ($$(PlutusTx.compile [|| mkWithdrawValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (factoryScriptHash owner)
    `PlutusTx.applyCode` PlutusTx.liftCode owner
    `PlutusTx.applyCode` PlutusTx.liftCode client)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

withdrawScriptHash :: PubKeyHash -> PubKeyHash -> ValidatorHash
withdrawScriptHash = (Scripts.validatorHash . ) . withdrawScript

_withdrawScript :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
_withdrawScript owner client = scriptHashAndBytes @PlutusScriptV1 $ withdrawScript (fromBS owner) (fromBS client)