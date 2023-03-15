{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.UniqueSeed where

import Cardano.Plutus.FromBS
import Cardano.Plutus.Common
import Cardano.Plutus.Validator
import Cardano.Plutus.V1.Contexts

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString as Strict hiding (head, split, length)
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
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Scripts (MintingPolicy, scriptCurrencySymbol)
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
import Ledger.Value 
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
import Cardano.Api.Shelley (PlutusScriptV1)

-- Ensure all assets from this input were burned
{-# INLINABLE ownAssetsBurned #-}
ownAssetsBurned :: ScriptContext -> Bool
ownAssetsBurned ctx@ScriptContext { scriptContextTxInfo = txIn } =
    geq zeroValue $ (allInput - adaInput) - burn
    where
        burn = fst $ split $ txInfoMint txIn
        adaInput = onlyValueOf Ada.adaSymbol Ada.adaToken allInput
        allInput = txOutValue $ txInInfoResolved ownInInput
        ownInInput = fromMaybeTraceError "Own input not found" (findOwnInput ctx)

{-# INLINABLE mkUniqueSeedValidator #-}
mkUniqueSeedValidator :: () -> () -> ScriptContext -> Bool
mkUniqueSeedValidator () () = ownAssetsBurned

data UniqueSeedValidator
instance Scripts.ValidatorTypes UniqueSeedValidator where
   type instance DatumType UniqueSeedValidator = ()
   type instance RedeemerType UniqueSeedValidator = ()

uniqueSeedScript :: Scripts.TypedValidator UniqueSeedValidator
uniqueSeedScript = Scripts.mkTypedValidator @UniqueSeedValidator
    $$(PlutusTx.compile [|| mkUniqueSeedValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

uniqueSeedScriptHash :: ValidatorHash
uniqueSeedScriptHash = Scripts.validatorHash uniqueSeedScript

-- Allows to mint token with AssetName matching signatory, with guarantee that minting transaction had only one output UTxO
{-# INLINABLE mkUniqueSeedPolicy #-}
mkUniqueSeedPolicy :: ValidatorHash -> () -> ScriptContext -> Bool
mkUniqueSeedPolicy _uniqueSeedScriptHash () ctx@ScriptContext{scriptContextTxInfo=txIn} =
    mintAllowed $ getOwnTokens ctx $ txInfoMint txIn
    where
        owner = TokenName $ getPubKeyHash $ head $ txInfoSignatories txIn
        mintAllowed [(name, qty)] =
            if name == owner
            then
                if qty == 1 then 1 == length (scriptOutputsAt _uniqueSeedScriptHash txIn)
                else qty == -1
            else False

uniqueSeedPolicy :: MintingPolicy
uniqueSeedPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . mkUniqueSeedPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode uniqueSeedScriptHash

uniqueSeedSymbol :: Value.CurrencySymbol
uniqueSeedSymbol = scriptCurrencySymbol uniqueSeedPolicy

_uniqueSeedScript :: Strict.ByteString
_uniqueSeedScript = scriptHashAndBytes @PlutusScriptV1 uniqueSeedScript

_uniqueSeedPolicy :: Strict.ByteString
_uniqueSeedPolicy = policyHashAndBytes @PlutusScriptV1 uniqueSeedPolicy