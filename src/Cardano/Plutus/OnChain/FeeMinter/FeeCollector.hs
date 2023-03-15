module Cardano.Plutus.OnChain.FeeMinter.FeeCollector where

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

data FeeCollectorValidator
instance ScriptsV2.ValidatorTypes FeeCollectorValidator where
   type instance DatumType FeeCollectorValidator = ()
   type instance RedeemerType FeeCollectorValidator = ()

-- Allows to spend input if owner UniqueNFT was spent, or referenced from address that signed this contract
-- Allows to spend input by anyone if value locked by this script increased
{-# INLINABLE mkFeeCollectorValidator #-}
mkFeeCollectorValidator :: CurrencySymbol -- ^ Owner's UniqueNFT policyId
                        -> ()
                        -> ()
                        -> ScriptContext -> Bool
mkFeeCollectorValidator ownerPolicy () () ctx@ScriptContext{scriptContextTxInfo=txIn} =
    if ownerSigned then True else assetsDebited
    where
        ownerSigned = txIn `spentOrReferencedValue` hasCurrency ownerPolicy
        assetsDebited = valueSpentFromScript (ownHash ctx) txIn `lt` valueLockedBy txIn (ownHash ctx)

feeCollectorScript :: CurrencySymbol -- ^ Owner's UniqueNFT policyId
                   -> ScriptsV2.TypedValidator FeeCollectorValidator
feeCollectorScript owner = ScriptsV2.mkTypedValidator @FeeCollectorValidator
    ($$(PlutusTx.compile [|| mkFeeCollectorValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode owner)
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedValidator ||])

feeCollectorScriptHash :: CurrencySymbol -> ValidatorHash
feeCollectorScriptHash = typedValidatorHash . feeCollectorScript

compilePlutusFeeCollector :: Strict.ByteString -> Strict.ByteString
compilePlutusFeeCollector a = scriptHashAndBytes @PlutusScriptV2 $ feeCollectorScript (fromBS a)