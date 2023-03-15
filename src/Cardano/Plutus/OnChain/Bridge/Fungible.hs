{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.Fungible where

import Cardano.Plutus.FromBS
import Cardano.Plutus.OnChain.Bridge.NFT
import Cardano.Plutus.Common
import Cardano.Plutus.Validator
import Cardano.Plutus.V1.Contexts

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString as Strict (ByteString)
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
import Plutus.Script.Utils.V1.Scripts (MintingPolicy, scriptCurrencySymbol)
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies
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

import Cardano.Api.Shelley (PlutusScriptV1)

-- Policy is parameterized by AssetName of owner NFTPolicy token
-- Can be minted or burned only if corresponding NFTPolicy token is present
-- Any AssetName is allowed for mint
mkFungiblePolicy :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
mkFungiblePolicy _nftSymbol nftName () ctx@ScriptContext { scriptContextTxInfo = info } =
    hasControlNft
    where
        spent = valueSpent info
        hasControlNft = Value.valueOf spent _nftSymbol nftName == 1
        -- NOTE: No explicit check for ControlNFT being present in output - so it could be burned by accident

fungiblePolicy :: TokenName -> MintingPolicy
fungiblePolicy nftName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| (mkUntypedMintingPolicy . ) . mkFungiblePolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode nftSymbol
    `PlutusTx.applyCode` PlutusTx.liftCode nftName

fungibleSymbol :: TokenName -> Value.CurrencySymbol
fungibleSymbol = scriptCurrencySymbol . fungiblePolicy

_fungiblePolicy :: Strict.ByteString -> Strict.ByteString
_fungiblePolicy = policyHashAndBytes @PlutusScriptV1 . fungiblePolicy . fromBS