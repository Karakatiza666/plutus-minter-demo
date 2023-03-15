
module Cardano.Plutus.OnChain.FeeMinter.UniqueNFT where

import Cardano.Plutus.Common

import PlutusTx.Prelude
import Prelude qualified as Haskell
import Cardano.Api.Shelley (PlutusScriptV2)
import Data.ByteString as Strict (ByteString)
import Cardano.Plutus.Common
import Cardano.Plutus.FromBS
import Cardano.Plutus.Validator
import Cardano.Plutus.V2.Contexts
import PlutusTx
import PlutusTx.Builtins
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Scripts
import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptsV2

-- Token which is unique for given PolicyId
-- Uniqueness guaranteed by consuming  UTxO with matching TxOutRef on mint, and being able to mint only one token for given policy
-- Belongs to a policy family parameterized by minting parent TxOutRef
-- Burning is allowed
-- Asset name can be freely customized
-- NFT can be renamed by burning and minting NFT with the same policy
mkUniqueNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkUniqueNFTPolicy seedUTxO () ctx@ScriptContext { scriptContextTxInfo = txIn } =
    -- TODO: add check to allow burn old and create new token of the same policy
    mintAllowed . getOwnTokens ctx $ txInfoMint txIn
    where
        mintAllowed [(_, tokenQty)] =
            if tokenQty == 1 then spentSeed
            else tokenQty == negate 1
        mintAllowed _ = False
        spentSeed = ((seedUTxO ==) . txInInfoOutRef) `any` txInfoInputs txIn

uniqueNFTPolicy :: TxOutRef -> MintingPolicy
uniqueNFTPolicy seedUTxO = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedMintingPolicy . mkUniqueNFTPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode seedUTxO

uniqueNFTSymbol :: TxOutRef -> CurrencySymbol
uniqueNFTSymbol = ScriptsV2.scriptCurrencySymbol . uniqueNFTPolicy

compilePlutusUniqueNFT :: Strict.ByteString -> Strict.ByteString
compilePlutusUniqueNFT a = policyHashAndBytes @PlutusScriptV2 $ uniqueNFTPolicy (fromBS a)

-- Can only be minted by the owner of some CurrencySymbol
mkWitnessPolicy :: CurrencySymbol -> () -> ScriptContext -> Bool
mkWitnessPolicy owner () ctx@ScriptContext { scriptContextTxInfo = txIn } =
    txIn `spentOrReferencedValue` hasCurrency owner

witnessPolicy :: CurrencySymbol -> MintingPolicy
witnessPolicy owner = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedMintingPolicy . mkWitnessPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode owner

witnessSymbol :: CurrencySymbol -> CurrencySymbol
witnessSymbol = ScriptsV2.scriptCurrencySymbol . witnessPolicy

compilePlutusWitness :: Strict.ByteString -> Strict.ByteString
compilePlutusWitness a = policyHashAndBytes @PlutusScriptV2 $ witnessPolicy (fromBS a)

uniqueWitnessPolicy :: TxOutRef -> MintingPolicy
uniqueWitnessPolicy seed = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedMintingPolicy . mkWitnessPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode (uniqueNFTSymbol seed)

uniqueWitnessSymbol :: TxOutRef -> CurrencySymbol
uniqueWitnessSymbol = ScriptsV2.scriptCurrencySymbol . uniqueWitnessPolicy

compilePlutusUniqueWitness :: Strict.ByteString -> Strict.ByteString
compilePlutusUniqueWitness a = policyHashAndBytes @PlutusScriptV2 $ uniqueWitnessPolicy (fromBS a)

parseTxOutRef :: Strict.ByteString -> Haskell.String
parseTxOutRef = Haskell.show . fromBS @TxOutRef
