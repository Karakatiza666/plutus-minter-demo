{-# OverloadedStrings #-}

-- User pays X worth of token + Cardano fee equivalent in token to access Minter functionality:

module Cardano.Plutus.OnChain.FeeMinter.Fungible where

import PlutusTx.Prelude
import Data.ByteString as Strict (ByteString)
import Cardano.Api.Shelley (PlutusScriptV2)
import Cardano.Plutus.FromBS
import Cardano.Plutus.Validator
import Cardano.Plutus.Common
import Cardano.Plutus.V2.Contexts
import PlutusTx
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Scripts

import Plutus.V2.Ledger.Contexts

import Data.ByteString as Strict (ByteString)
import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptsV2

import Cardano.Plutus.OnChain.FeeMinter.UniqueNFT
import Cardano.Plutus.OnChain.FeeMinter.FeeCollector
import Cardano.Plutus.OnChain.FeeMinter.TokenOracle
import Cardano.Api.Shelley (PlutusScriptV2)
import Control.Monad (ap, liftM2)

-- TxOutRef is passed at runtime
-- CurrencySymbol is passed at runtime
-- Policy is parameterized by AssetName of owner NFTPolicy token
-- Can be minted or burned only if corresponding NFTPolicy token is present
-- Any AssetName is allowed for mint, but is later fixed
{-# INLINABLE mkFungiblePolicy #-}
mkFungiblePolicy :: CurrencySymbol -- ^ of TokenOracle
                      -> CurrencySymbol -- ^ of UniqueNFT for fungible control
                      -> ValidatorHash -- ^ of Collector UTxO
                      -> TxOutRef -- ^ of fungible seed UTxO
                      -> Maybe OracleRedeemer
                      -> ScriptContext
                      -> Bool
mkFungiblePolicy mintOracle uniqueNft feeCollector seedUTxO mRedeemer ctx@ScriptContext { scriptContextTxInfo = txIn } =
    if firstMint then oracleFeePaid else hasControlNft
    where
        firstMint = ((seedUTxO ==) . txInInfoOutRef) `any` txInfoInputs txIn
        oracleFeePaid = or do
            OracleRedeemer{feeAcl, feeRef} <- mRedeemer
            fee <- oracleAmount feeAcl feeRef
            let paidFee = aclTransferredTo feeAcl feeCollector txIn
            return $ paidFee >= fee 
            where
                feeValueIn = valueOf' mintOracle . tokenOracleName "fungible"
                oracleAmount acl ref = feeValueIn acl . txOutValue <$> referenceAt ref txIn
        hasControlNft = txIn `spentOrReferencedValue` hasCurrency uniqueNft
        -- NOTE: No explicit check for ControlNFT being present in output - so it could be burned by accident

minterFungiblePolicy :: CurrencySymbol -- ^ Admin UniqueNFT
                    -> TxOutRef -- ^ Seed UTxO of fungible mint
                    -> MintingPolicy
minterFungiblePolicy adminNft seedUTxO = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \ a b c d -> ScriptsV2.mkUntypedMintingPolicy $ mkFungiblePolicy a b c d ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (tokenOracleSymbol adminNft)
    `PlutusTx.applyCode` PlutusTx.liftCode (uniqueNFTSymbol seedUTxO)
    `PlutusTx.applyCode` PlutusTx.liftCode (feeCollectorScriptHash adminNft)
    `PlutusTx.applyCode` PlutusTx.liftCode seedUTxO

minterFungibleSymbol :: CurrencySymbol -- ^ Admin UniqueNFT
                    -> TxOutRef -- ^ Seed UTxO
                    -> CurrencySymbol
minterFungibleSymbol adminNft seedUTxO = ScriptsV2.scriptCurrencySymbol $ minterFungiblePolicy adminNft seedUTxO

compilePlutusFungible :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
compilePlutusFungible a b = policyHashAndBytes @PlutusScriptV2 $ minterFungiblePolicy (fromBS a) (fromBS b)