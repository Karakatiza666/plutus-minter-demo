{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.WithdrawFactory where

import Cardano.Plutus.Validator
import Cardano.Api.Shelley (PlutusScriptV1)
import Cardano.Plutus.FromBS
import Cardano.Plutus.Common
import Cardano.Plutus.V1.Contexts

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

data FactoryValidator
instance Scripts.ValidatorTypes FactoryValidator where
   type instance DatumType FactoryValidator = ()
   type instance RedeemerType FactoryValidator = ()

-- Owner can create, dissolve, deposit ADA to and from the factory, and move ADA between factories
-- Factory UTxO can be transferred to another owner
mkFactoryValidator :: PubKeyHash -> () -> () -> ScriptContext -> Bool
mkFactoryValidator owner () () ctx@ScriptContext{scriptContextTxInfo=txIn} =
    signedByOwner || adaReturned
    where
        signedByOwner = Validation.txSignedBy txIn owner
        scriptInputsAda = foldr ((<>) . txOutValue . txInInfoResolved) PP.mempty (scriptTxInsThat txIn (const True))
        ownOut = maybeOnly $ getContinuingOutputs ctx
        adaReturned = or $ (scriptInputsAda ==) . txOutValue <$> ownOut

factoryScript :: PubKeyHash -> Scripts.TypedValidator FactoryValidator
factoryScript owner = Scripts.mkTypedValidator @FactoryValidator
    ($$(PlutusTx.compile [|| mkFactoryValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode owner)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

factoryScriptHash :: PubKeyHash -> ValidatorHash
factoryScriptHash = Scripts.validatorHash . factoryScript

_factoryScript :: Strict.ByteString -> Strict.ByteString
_factoryScript = scriptHashAndBytes @PlutusScriptV1 . factoryScript . fromBS