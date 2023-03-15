{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Plutus.OnChain.FeeMinter.Test where

import Cardano.Plutus.OnChain.FeeMinter.FeeExchangeCollectorDatum
import Cardano.Plutus.OnChain.FeeMinter.TokenOracle
import Cardano.Plutus.Test
import Data.Text
import Data.Text.Encoding
import Prelude
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Data.ByteString qualified as Strict
import PlutusTx.IsData.Class

fooCollectorDatum = ExchangeAda
        (CurrencySymbol "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd")
        (TokenName "506577446965506965")
        256

oracleRedeemer = OracleRedeemer {
    feeAcl = AssetClass ((CurrencySymbol "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"), (TokenName "506577446965506965")),
    feeRef = TxOutRef "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd" 3
}

showMaybeBS :: Maybe Strict.ByteString -> Strict.ByteString
showMaybeBS Nothing = "Nothing"
showMaybeBS (Just bs) = "Just " <> bs

thereAndBack :: forall a. (FromData a, ToData a) => a -> Text
thereAndBack = decodeUtf8 . showMaybeBS . (dataToJson @a <$>) . jsonToData . dataToJson

testCollectorDatum = thereAndBack $ fooCollectorDatum
testOracle = thereAndBack $ oracleRedeemer
testEmpty = thereAndBack $ ()
