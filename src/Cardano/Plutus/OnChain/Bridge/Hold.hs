{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.Hold where

import Cardano.Api.Shelley (PlutusScriptV1)
import Cardano.Plutus.FromBS
import Cardano.Plutus.OnChain.Bridge.NonceInteger
import Cardano.Plutus.Common
import Cardano.Plutus.Validator
import Cardano.Plutus.V1.Contexts

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString as Strict (ByteString(..))
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Tuple.Extra hiding (fst, snd, uncurry)
import Data.Void (Void)
import Data.Coerce (coerce)


import Ledger.Ada qualified as Ada
import Ledger.Address
import Ledger.Credential (Credential(..))
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain ()


import Plutus.V1.Ledger.Contexts 
import Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Api 
import Ledger.Value 
import Ledger.Value qualified as Value
import Ledger.TimeSlot qualified as TimeSlot
-- import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test hiding (not)

import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
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

newtype HoldTransferRedeemer = HoldTransferRedeemer {
    newOwner :: ValidatorHash
}
PlutusTx.unstableMakeIsData ''HoldTransferRedeemer
PlutusTx.makeLift ''HoldTransferRedeemer

data HoldValidator
instance Scripts.ValidatorTypes HoldValidator where
   type instance DatumType HoldValidator = ()
   type instance RedeemerType HoldValidator = Maybe HoldTransferRedeemer

-- Hold UTxO is used for holding Cardano Native Tokens (CNT) when they are transferred to exchange
-- Hold UTxO should only have one type of CNT at the same time
-- Hold UTxO cannot be used until all extra CNTs are sent to other Hold UTxOs
-- Assets can be sent to the owner, preserving the UTxO
-- Hold UTxO can be split into two
-- Hold UTxO can be dissolved if all its CNTs were sent to other Hold UTxOs or owner
-- When CNTs are sent not to owner or other Hold UTxO - NonceValidator UTxO needs to be one of the inputs
-- Ownership over Hold UTxO can be transferred to another owner
mkHoldValidator :: ValidatorHash -> PubKeyHash -> () -> Maybe HoldTransferRedeemer -> ScriptContext -> Bool
mkHoldValidator _nonceScriptHash owner () redeemer ctx@ScriptContext{scriptContextTxInfo=txIn} =
    maybe
        (if signedByOwner
            then (if isSingleAssetOut then outsNonceValidated else False)
            else (if isOnlyRun then isDepositOk else True))
        ((signedByOwner &&) . ownerTransferOk)
        redeemer
    where
        signedByOwner = txSignedBy txIn owner
        ownOuts = getContinuingOutputs ctx
        ownInInput = fromMaybeTraceError "Own input ot found" (findOwnInput ctx)
        ownInput = txInInfoResolved ownInInput
        ownInInputs = scriptTxInsThat txIn (ownHash ctx ==)
        ownInputs = txInInfoResolved <$> ownInInputs
        ownerTransferOk HoldTransferRedeemer {..} =
            if length ownOuts == 0
            then if length newOuts > 0
            then onlyPaidTo txIn owner -- avoid accidentally sending money to client and forgetting to update nonce
            else False
            else False
            where
                newOuts = scriptOutputsAt newOwner txIn
                -- newOutsSum = foldr ((<>) . snd) mempty newOuts
        {-# INLINABLE isSingleAsset #-}
        isSingleAsset = (<= 2) . valueAssetsQty . txOutValue
        isSingleAssetOut = and $ isSingleAsset `map` ownOuts
        {-# INLINABLE isSingleAssetIn #-}
        isSingleAssetIn = isSingleAsset ownInput
        notOwnOuts = scriptOutputsThat txIn (ownHash ctx /=)
        nonceIns = scriptTxInsThat txIn (_nonceScriptHash ==)
        -- If sending CNTs somewhere apart from other hold UTxO or owner - nonce validator must be present in inputs (so it is executed)
        -- CNTs can be sent to other hold UTxOs or owner to clear UTxO of extra CNTs
        outsNonceValidated = 
            if (if length notOwnOuts == 0 then onlyPaidTo txIn owner else False)
            then True
            else (if isSingleAssetIn then length nonceIns > 0 else False)
        -- Allows to run code only once for all UTxOs of certain script
        -- Assumes txInfoInputs are always in the same order
        isOnlyRun = on (==) txInInfoOutRef ownInInput (head ownInInputs)
        -- Ensures assets (or ADA, if no other assets) are only added to each of the UTxOs
        -- Also ensures there is only one asset apart from ADA in each UTxO
        isDepositOk =
            if length result == length ownInputs then and result else False
            where
                result = on comparePair (sort . mapMaybe (assetPair . txOutValue)) ownOuts ownInputs
                comparePair o i = map (uncurry (&&)) $ zipWith ((==) `biliftA2` (>=)) o i
                -- Returns Nothing if there is more than one asset other than ADA
                -- Returns (0, ADA) if only ADA is present
                -- Returns (ADA, QTY) otherwise, where QTY is quantity of non-ADA asset
                assetPair v =
                    let
                        assets = flattenValue v
                        isOnlyAda = length assets == 1
                        adaAsset = if isOnlyAda then 0 else valueOf v Ada.adaSymbol Ada.adaToken
                        otherAsset = fmap thd3 . maybeOnly $ (if isOnlyAda then id else filter ((Ada.adaSymbol /=) . fst3)) assets
                    in (adaAsset,) <$> otherAsset

holdScript :: PubKeyHash -> Scripts.TypedValidator HoldValidator
holdScript owner = Scripts.mkTypedValidator @HoldValidator
    ($$(PlutusTx.compile [|| mkHoldValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (nonceScriptHash owner)
    `PlutusTx.applyCode` PlutusTx.liftCode owner)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

holdScriptHash :: PubKeyHash -> ValidatorHash
holdScriptHash = Scripts.validatorHash . holdScript

_holdScript :: Strict.ByteString -> Strict.ByteString
_holdScript = scriptHashAndBytes @PlutusScriptV1 . holdScript . fromBS