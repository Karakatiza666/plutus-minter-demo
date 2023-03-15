{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.OnChain.Bridge.NFT where

import Cardano.Plutus.FromBS
import Cardano.Plutus.OnChain.Bridge.UniqueSeed
import Cardano.Plutus.Common
import Cardano.Plutus.Validator
import Cardano.Plutus.V1.Contexts

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString as Strict hiding (head, split, length, map)
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

-- Token whose AssetName in a domain of this policy is guaranteed to be unique - by uniqueness of transaction Id
-- NFT that gives control over minting of a family of currency policies
-- Can only be minted from UniqueSeedScript UTxO, and minted AssetName matches UniqueSeedScript UTxO transaction Id
-- Burning is allowed
mkNFTPolicy :: CurrencySymbol -> () -> ScriptContext -> Bool
mkNFTPolicy _uniqueSeedSymbol () ctx@ScriptContext { scriptContextTxInfo = txIn } =
    and . map mintAllowed . getOwnTokens ctx $ txInfoMint txIn
    where
        owner = TokenName $ getPubKeyHash $ head $ txInfoSignatories txIn
        mintAllowed (tokenName, tokenQty) =
            if tokenQty == 1 then consumedSeed
            else tokenQty == negate 1
            where
                consumedSeed =
                    flip consumedUTxO txIn \TxInInfo {..} ->
                        if valueOf (txInfoMint txIn) _uniqueSeedSymbol owner == -1
                        then unTokenName tokenName == outTxId txInInfoOutRef
                        else False

nftPolicy :: MintingPolicy
nftPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkUntypedMintingPolicy . mkNFTPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode uniqueSeedSymbol

nftSymbol :: CurrencySymbol
nftSymbol = scriptCurrencySymbol nftPolicy

_nftPolicy :: Strict.ByteString
_nftPolicy = policyHashAndBytes @PlutusScriptV1 nftPolicy