{-# LANGUAGE DeriveGeneric #-}

-- Oracle that provides price for Token services
-- Holder of owner UniqueNFT can freely mint tokens with any TokenName
-- PolicyId is determined by owner, and token used for payment for Token service
-- TokenName is blake2b hash of oracle topic, and policyId and tokenName in question
-- Token amount describes how much target token is to be paid for Token service

module Cardano.Plutus.OnChain.FeeMinter.TokenOracle where


import PlutusTx.Prelude

import Cardano.Api.Shelley (PlutusScriptV2)
import Data.ByteString as Strict (ByteString)
import GHC.Generics
import Data.ByteString as Strict (ByteString)
import Cardano.Plutus.Validator
import Cardano.Plutus.Common
import Cardano.Plutus.FromBS
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



data OracleRedeemer = OracleRedeemer {
    feeAcl :: AssetClass,
    feeRef :: TxOutRef
} deriving stock Generic
makeIsDataIndexed ''OracleRedeemer [('OracleRedeemer, 0)]
makeLift ''OracleRedeemer

{-# INLINABLE mkTokenOraclePolicy #-}
mkTokenOraclePolicy :: CurrencySymbol -- ^ Owner UniqueNFT
                    -> ()
                    -> ScriptContext
                    -> Bool
mkTokenOraclePolicy ownerNft () ctx@ScriptContext { scriptContextTxInfo = txIn } =
    if authorized `any` txInfoReferenceInputs txIn
    then True
    else authorized `any` txInfoInputs txIn
    where
        authorized = hasCurrency ownerNft . txOutValue . txInInfoResolved

tokenOraclePolicy :: CurrencySymbol -- ^ Owner UniqueNFT
                  -> MintingPolicy
tokenOraclePolicy ownerNft  = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \ ownerNft -> ScriptsV2.mkUntypedMintingPolicy $ mkTokenOraclePolicy ownerNft ||])
    `PlutusTx.applyCode` PlutusTx.liftCode ownerNft

tokenOracleSymbol :: CurrencySymbol -- ^ Owner UniqueNFT
                  -> CurrencySymbol
tokenOracleSymbol ownerNft = ScriptsV2.scriptCurrencySymbol $ tokenOraclePolicy ownerNft

tokenOracleName :: BuiltinByteString -> AssetClass -> TokenName
tokenOracleName topic (AssetClass (policy, token)) =
    TokenName $ blake2b_256 $ topic `appendByteString` unCurrencySymbol policy `appendByteString` unTokenName token

compilePlutusTokenOracle :: Strict.ByteString -> Strict.ByteString
compilePlutusTokenOracle a = policyHashAndBytes @PlutusScriptV2 $ tokenOraclePolicy (fromBS a)