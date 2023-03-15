{-# OverloadedStrings #-}

-- Lets you mint a limited NFT collection
-- You mint and lock unique seed in an NFTFactory script
-- You can then spend NFTFactory script to mint batches of NFTs with the same policyId, while incrementing factory counter
-- When you want you can finish the collection by destroying the factory and associated seed

module Cardano.Plutus.OnChain.FeeMinter.NFTCollection where

import PlutusTx.Prelude

import Data.Monoid (Sum(..), getSum)
import Data.ByteString as Strict (ByteString)

import Cardano.Api.Shelley (PlutusScriptV2)
import Cardano.Plutus.Validator
import Cardano.Plutus.Common
import Cardano.Plutus.FromBS
import Cardano.Plutus.V2.Contexts
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.AssocMap qualified as PMap
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Scripts
import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptsV2 hiding (validatorHash)

import Cardano.Plutus.OnChain.FeeMinter.TokenOracle
import Cardano.Plutus.OnChain.FeeMinter.FeeCollector
import Cardano.Plutus.OnChain.FeeMinter.UniqueNFT


data NFTFactoryValidator
instance ScriptsV2.ValidatorTypes NFTFactoryValidator where
   type instance DatumType NFTFactoryValidator = Integer
   type instance RedeemerType NFTFactoryValidator = Maybe OracleRedeemer

-- Can only be dissolved if seed is dissolved
-- seedPolicy should only allow single TokenName
{-# INLINABLE mkNFTFactoryValidator #-}
mkNFTFactoryValidator :: CurrencySymbol -- ^ Admin Oracle
                           -> CurrencySymbol -- ^ Factory UniqueNFT
                           -> ValidatorHash -- ^ of Collector UTxO
                           -> Integer
                           -> Maybe OracleRedeemer
                           -> ScriptContext
                           -> Bool
mkNFTFactoryValidator mintOracle seedPolicy feeCollector oldIdx mRedeemer ctx@ScriptContext{scriptContextTxInfo=txIn} =
    -- if feePerMintPaid then seedImprisoned else False
    or $ seedImprisoned <$> feePerMintPaid
    where
        -- Retrieves new counter value of minted tokens
        feePerMintPaid = do
            let minted = PMap.toList $ PMap.delete seedPolicy $ getValue $ txInfoMint txIn
            case minted of
                [] -> Just oldIdx
                [(_, tokenMap)] -> do -- Only allow mint of a single policyId
                    OracleRedeemer{feeAcl, feeRef} <- mRedeemer
                    fee <- oracleAmount feeAcl feeRef
                    let
                        mintQty = length $ PMap.elems tokenMap
                        paidFee = aclTransferredTo feeAcl feeCollector txIn
                    if paidFee >= fee * mintQty
                    then Just $ oldIdx + mintQty
                    else Nothing
                _ -> Nothing
            where
                nonZero x = if x == 0 then Nothing else Just x
                feeValueIn = valueOf' mintOracle . tokenOracleName "nft_collection"
                oracleAmount acl ref = feeValueIn acl . txOutValue <$> referenceAt ref txIn
        -- As we expect seedPolicy to be unique this will only allow single out with seed, or burning seed and dissolving factory
        seedImprisoned newIdx = case getContinuingOutputs ctx of
            [] -> or $ ((-1 ==) . foldl (+) 0 . toList) <$> (PMap.lookup seedPolicy $ getValue $ txInfoMint txIn) -- Seed burned if no continuing output
            [out] -> if hasCurrency seedPolicy (txOutValue out)
                then or $ (newIdx ==) <$> inlineDatum (txOutDatum out)
                else False
            _ -> False

minterNFTFactoryScript :: CurrencySymbol -- ^ Admin UniqueNFT policyId
                      -> TxOutRef
                      -> ScriptsV2.TypedValidator NFTFactoryValidator
minterNFTFactoryScript owner seedUTxO =
    let uniqueNft = uniqueNFTSymbol seedUTxO
    in ScriptsV2.mkTypedValidator @NFTFactoryValidator
    (
        $$(PlutusTx.compile [|| \a b c -> mkNFTFactoryValidator a b c ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (tokenOracleSymbol owner)
        `PlutusTx.applyCode` PlutusTx.liftCode uniqueNft
        `PlutusTx.applyCode` PlutusTx.liftCode (feeCollectorScriptHash uniqueNft)
    )
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedValidator ||])

minterNFTFactoryScriptHash :: CurrencySymbol -> TxOutRef -> ValidatorHash
minterNFTFactoryScriptHash a b = typedValidatorHash $ minterNFTFactoryScript a b

compilePlutusNFTFactory :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
compilePlutusNFTFactory a b = scriptHashAndBytes @PlutusScriptV2 $ minterNFTFactoryScript (fromBS a) (fromBS b)

mkNFTCollectionPolicy :: ValidatorHash -- ^ of NFTFactory
                           -> ()
                           -> ScriptContext
                           -> Bool
mkNFTCollectionPolicy factoryScript () ctx@ScriptContext { scriptContextTxInfo = txIn } =
    and . map mintAllowed . getOwnTokens ctx $ txInfoMint txIn
    where
        mintAllowed (tokenName, tokenQty) =
            if tokenQty == 1 then spentFactory
            else tokenQty == negate 1
        spentFactory = spentScriptInput factoryScript txIn

minterNFTCollectionPolicy :: CurrencySymbol -- ^ Admin UniqueNFT policyId
                         -> TxOutRef
                         -> MintingPolicy
minterNFTCollectionPolicy adminNft seedUTxO = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \ a -> ScriptsV2.mkUntypedMintingPolicy $ mkNFTCollectionPolicy a ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (minterNFTFactoryScriptHash adminNft seedUTxO)

compilePlutusNFTCollection :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
compilePlutusNFTCollection a b = policyHashAndBytes @PlutusScriptV2 $ minterNFTCollectionPolicy (fromBS a) (fromBS b)
