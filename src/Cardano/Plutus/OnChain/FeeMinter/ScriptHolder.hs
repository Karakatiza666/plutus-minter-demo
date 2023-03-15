module Cardano.Plutus.OnChain.FeeMinter.ScriptHolder where

import PlutusTx.Prelude

import Data.ByteString as Strict (ByteString)
import Cardano.Api.Shelley (PlutusScriptV2)
import Cardano.Plutus.Validator
import Cardano.Plutus.Common
import Cardano.Plutus.FromBS
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
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptsV2 hiding (validatorHash)

data ScriptHolderValidator
instance ScriptsV2.ValidatorTypes ScriptHolderValidator where
   type instance DatumType ScriptHolderValidator = ()
   type instance RedeemerType ScriptHolderValidator = ()

-- UTxO used to store reference scripts
-- Accepts dummy parameter to make policy parameterizable by desired script to improve lookup
{-# INLINABLE mkScriptHolderValidator #-}
mkScriptHolderValidator :: CurrencySymbol -- ^ UniqueSeed witness -- Spending owner
                        -> ScriptHash -- ^ Reference Script
                        -> ()
                        -> ()
                        -> ScriptContext -> Bool
mkScriptHolderValidator ownerWitness scriptHash () () ctx@ScriptContext{scriptContextTxInfo=txIn} =
    -- Can only be spent if UTxO contained owner witness token
    if hasCurrency ownerWitness $ txOutValue $ txInInfoResolved $ requireOwnInput ctx
    -- Can only be spent if similar UTxOs have the inline script
    then ((Just scriptHash ==) . txOutReferenceScript) `all` getContinuingOutputs ctx
    else False

scriptHolderScript :: CurrencySymbol -- ^ Owner's UniqueNFT policyId
                   -> ScriptHash
                   -> ScriptsV2.TypedValidator ScriptHolderValidator
scriptHolderScript ownerWitness scriptHash = ScriptsV2.mkTypedValidator @ScriptHolderValidator
    ($$(PlutusTx.compile [|| mkScriptHolderValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode ownerWitness
    `PlutusTx.applyCode` PlutusTx.liftCode scriptHash)
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedValidator ||])

scriptHolderScriptHash :: CurrencySymbol -> ScriptHash -> ValidatorHash
scriptHolderScriptHash a b = typedValidatorHash $ scriptHolderScript a b

compilePlutusScriptHolder :: Strict.ByteString -> Strict.ByteString -> Strict.ByteString
compilePlutusScriptHolder a b = scriptHashAndBytes @PlutusScriptV2 $ scriptHolderScript (fromBS a) (fromBS b)