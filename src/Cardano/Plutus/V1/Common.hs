{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.V1.Common where

import Codec.Serialise
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, textEnvelopeRawCBOR, serialiseToTextEnvelope)
import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString.Short qualified as SBS
import Data.ByteString.Lazy qualified as LB
import Data.ByteString qualified as SB
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)

import Ledger
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
import Ledger.Tx qualified as Tx
import Plutus.Script.Utils.V1.Scripts (MintingPolicy)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
import Ledger.Value
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test hiding (not)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold, foldr)
import PlutusTx.Prelude qualified as PP
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins hiding (foldr)
import PlutusTx.Foldable (foldl, foldr)
import PlutusTx.Numeric ()
import PlutusTx.Maybe (maybe)
import Data.Semigroup (Sum (Sum), getSum)
import Prelude qualified as Haskell
import Schema (ToSchema)
import Control.Arrow ((&&&))

{-# INLINABLE readRawDatum #-}
readRawDatum :: PlutusTx.FromData a => Datum -> Maybe a
readRawDatum = PlutusTx.fromBuiltinData . getDatum

{-# INLINABLE readDatum #-}
readDatum :: PlutusTx.FromData a => TxInfo -> DatumHash -> Maybe a
readDatum txIn = readRawDatum <=< flip findDatum txIn