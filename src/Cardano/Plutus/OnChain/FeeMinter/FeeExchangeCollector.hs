{-# LANGUAGE DeriveGeneric #-}

module Cardano.Plutus.OnChain.FeeMinter.FeeExchangeCollector where

import PlutusTx.Prelude

import Data.ByteString as Strict (ByteString)
import Cardano.Api.Shelley (PlutusScriptV2)
import Cardano.Plutus.Validator
import Cardano.Plutus.Common
import Cardano.Plutus.FromBS
import Cardano.Plutus.V2.Contexts
import GHC.Generics
import PlutusTx
import PlutusTx.Builtins
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Scripts
import Ledger.Ada hiding (adaSymbol, adaToken)

import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptsV2 hiding (validatorHash)

import Cardano.Plutus.OnChain.FeeMinter.FeeExchangeCollectorDatum

data FeeExchangeCollectorValidator
instance ScriptsV2.ValidatorTypes FeeExchangeCollectorValidator where
   type instance DatumType FeeExchangeCollectorValidator = FeeExchangeDatum
   type instance RedeemerType FeeExchangeCollectorValidator = ()

-- Allows to spend input if owner UniqueNFT was spent, or referenced from address that signed this contract
-- Allows to spend input by anyone if value locked by this script doesn't change, and datum is unchanged
-- Non-owner can only spend one script UTxO per tx
-- Some token can be exchanged for Ada with specified exchangeRate
-- Token can be exchanged even if it not used to pay transaction fee
{-# INLINABLE mkFeeExchangeCollectorValidator #-}
mkFeeExchangeCollectorValidator :: CurrencySymbol -- ^ Owner's UniqueNFT policyId
                        -> FeeExchangeDatum
                        -> ()
                        -> ScriptContext -> Bool
mkFeeExchangeCollectorValidator ownerPolicy exchangeRate () ctx@ScriptContext{scriptContextTxInfo=txIn} =
    if ownerSigned then True else if assetsDebited then collectorIntact else False
    where
        ownerSigned = txIn `spentOrReferencedValue` hasCurrency ownerPolicy
        valueSpent = valueSpentFromScript (ownHash ctx) txIn
        valueLocked = valueLockedBy txIn (ownHash ctx)
        assetsDebited =
            -- How much Ada collector gave away, and how many tokens received
            let exchange = case exchangeRate of
                    NoExchange -> mempty
                    ExchangeAda policyId assetName perAda ->
                        let
                            lostAda = on (-) (getLovelace . fromValue) valueSpent valueLocked
                        -- Only allow one way exchange of token for ada
                            qty = (lostAda `max` 0) * perAda `divideInteger` 1000000
                        in singleton adaSymbol adaToken (negate lostAda) <> singleton policyId assetName qty
            in (valueSpent <> exchange) `leq` valueLocked
        collectorIntact =
            if hasOnlyOwnDatum ctx
            then hasOnlyScriptInAt (ownHash ctx) txIn
            else False

feeExchangeCollectorScript :: CurrencySymbol -- ^ Owner's UniqueNFT policyId
                           -> ScriptsV2.TypedValidator FeeExchangeCollectorValidator
feeExchangeCollectorScript owner = ScriptsV2.mkTypedValidator @FeeExchangeCollectorValidator
    ($$(PlutusTx.compile [|| mkFeeExchangeCollectorValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode owner)
    $$(PlutusTx.compile [|| ScriptsV2.mkUntypedValidator ||])

feeExchangeCollectorScriptHash :: CurrencySymbol -> ValidatorHash
feeExchangeCollectorScriptHash = typedValidatorHash . feeExchangeCollectorScript

compilePlutusFeeExchangeCollector :: Strict.ByteString -> Strict.ByteString
compilePlutusFeeExchangeCollector a = scriptHashAndBytes @PlutusScriptV2 $ feeExchangeCollectorScript (fromBS a)