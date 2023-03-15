{-# OverloadedStrings #-}

module Cardano.Plutus.OnChain.FeeMinter.NFT where

import PlutusTx.Prelude

import Data.ByteString as Strict (ByteString)
import Cardano.Api.Shelley (PlutusScriptV2)
import Cardano.Plutus.Common
import Cardano.Plutus.FromBS
import Cardano.Plutus.Validator
import Cardano.Plutus.V2.Contexts
import PlutusTx
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Scripts
import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptsV2

import Cardano.Plutus.OnChain.FeeMinter.TokenOracle
import Cardano.Plutus.OnChain.FeeMinter.UniqueNFT
import Cardano.Plutus.OnChain.FeeMinter.FeeCollector

-- Token which is unique for given PolicyId
-- Uniqueness guaranteed by consuming  UTxO with matching TxOutRef on mint, and being able to mint only one token for given policy
-- Belongs to a policy family parameterized by minting parent TxOutRef
-- Burning is allowed
-- Asset name can be freely customized
-- NFT cannot be renamed
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: CurrencySymbol -- ^ Admin Oracle
                 -> ValidatorHash -- ^ of Collector UTxO
                 -> TxOutRef -- ^ User seed
                 -> Maybe OracleRedeemer
                 -> ScriptContext
                 -> Bool
mkNFTPolicy mintOracle feeCollector seedUTxO mRedeemer ctx@ScriptContext { scriptContextTxInfo = txIn } =
    mintAllowed . getOwnTokens ctx $ txInfoMint txIn
    where
        mintAllowed [(_, tokenQty)] =
            if tokenQty == 1
            then if spentSeed then oracleFeePaid else False
            else tokenQty == negate 1
        mintAllowed _ = False
        spentSeed = spentTxOutRef seedUTxO txIn
        oracleFeePaid = or do
            OracleRedeemer{feeAcl, feeRef} <- mRedeemer
            fee <- oracleAmount feeAcl feeRef
            let paidFee = aclTransferredTo feeAcl feeCollector txIn
            return $ paidFee >= fee 
            where
                feeValueIn = valueOf' mintOracle . tokenOracleName "nft"
                oracleAmount acl ref = feeValueIn acl . txOutValue <$> referenceAt ref txIn

minterNFTPolicy :: CurrencySymbol -- ^ Admin UniqueNFT
               -> TxOutRef
               -> MintingPolicy
minterNFTPolicy adminNft seedUTxO = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \ a b c -> ScriptsV2.mkUntypedMintingPolicy $ mkNFTPolicy a b c ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (tokenOracleSymbol adminNft)
    `PlutusTx.applyCode` PlutusTx.liftCode (feeCollectorScriptHash adminNft)
    `PlutusTx.applyCode` PlutusTx.liftCode seedUTxO

minterNFTSymbol :: TxOutRef -> CurrencySymbol
minterNFTSymbol = ScriptsV2.scriptCurrencySymbol . uniqueNFTPolicy

compilePlutusNFT :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
compilePlutusNFT a b = policyHashAndBytes @PlutusScriptV2 $ minterNFTPolicy (fromBS a) (fromBS b)