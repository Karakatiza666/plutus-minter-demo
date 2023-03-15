{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.NonceSHA3_256 where

import Cardano.Api.Shelley (PlutusScriptV1)
import Cardano.Plutus.FromBS
import Cardano.Plutus.Common
import Cardano.Plutus.Validator
import Cardano.Plutus.V1.Common
import Cardano.Plutus.V1.Contexts
import Cardano.Plutus.List

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString.Lazy qualified as Lazy
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


import Plutus.V1.Ledger.Contexts 
import Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Api 
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
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

type EvmNonce = BuiltinByteString

{-# INLINABLE nextNonce #-}
nextNonce :: EvmNonce -> BuiltinByteString -> EvmNonce
nextNonce = (sha3_256 .) . appendByteString

newtype NonceDatum = NonceDatum {
    evmNonce :: EvmNonce
} 
    deriving stock Generic
--     deriving anyclass Eq
instance Eq NonceDatum where
    {-# INLINABLE (==) #-}
    a == b = evmNonce a == evmNonce b
PlutusTx.unstableMakeIsData ''NonceDatum

data NonceRedeemer = NonceRedeemer {
    newEvmNonce :: EvmNonce,
    evmTxs :: [BuiltinByteString]
} deriving stock Generic
PlutusTx.unstableMakeIsData ''NonceRedeemer
PlutusTx.makeLift ''NonceRedeemer

newtype NonceTransferRedeemer = NonceTransferRedeemer {
    newOwner :: ValidatorHash
}
PlutusTx.unstableMakeIsData ''NonceTransferRedeemer
PlutusTx.makeLift ''NonceTransferRedeemer

data NonceValidator
instance Scripts.ValidatorTypes NonceValidator where
   type instance DatumType NonceValidator = NonceDatum
   type instance RedeemerType NonceValidator = Maybe (Either NonceTransferRedeemer NonceRedeemer)

-- Nonce UTxO contains minUTxOAda with Datum that signifies last valid transaction nonce
-- To update Datum owner must submit a new nonce and a qty that is required to evolve existing nonce to a new nonce
-- Ownership over nonce UTxO can be transferred to another wallet
-- Nonce UTxO can be dissolved by reclaiming funds to the owner wallet
{-# INLINABLE mkNonceValidator #-}
mkNonceValidator :: PubKeyHash -> NonceDatum -> Maybe (Either NonceTransferRedeemer NonceRedeemer) -> ScriptContext -> Bool
mkNonceValidator owner datum@NonceDatum{..} redeemer ctx@ScriptContext{scriptContextTxInfo=txIn} =
    if signedByOwner then maybe dissolveOk (either ownerTransferOk nonceUpdateOk) redeemer else False
    where
        signedByOwner = txSignedBy txIn owner
        dissolveOk = valuePaidTo txIn owner == valueProduced txIn -- foldl (<>) PP.mempty (valueProduced txIn)
        -- TODO:  enable transfering multiple UTxOs of the same script at the same time
        ownerTransferOk NonceTransferRedeemer {..} =
            if length (getContinuingOutputs ctx) == 0
            then if hasOnlyDatumAt txIn (datum ==) newOwner
            then onlyPaidTo txIn owner -- to avoid accidentally sending money to client and forgetting to update nonce
            else False
            else False
        nonceUpdateOk NonceRedeemer{..} = redeemerMatchesDatum && hasOutDatum
            where
                redeemerMatchesDatum = newEvmNonce == foldl nextNonce evmNonce evmTxs
                hasOutDatum = isJust $ ((newEvmNonce ==) . (\(NonceDatum d) -> d)) `onlyAndSatisfies` mapMaybe (readDatum txIn <=< txOutDatumHash) (getContinuingOutputs ctx)

nonceScript :: PubKeyHash -> Scripts.TypedValidator NonceValidator
nonceScript owner = Scripts.mkTypedValidator @NonceValidator
    ($$(PlutusTx.compile [|| mkNonceValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode owner)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

nonceScriptHash :: PubKeyHash -> ValidatorHash
nonceScriptHash = Scripts.validatorHash . nonceScript

_nonceScript :: Strict.ByteString -> Strict.ByteString
_nonceScript = scriptHashAndBytes @PlutusScriptV1 . nonceScript . fromBS