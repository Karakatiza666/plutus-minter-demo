
module Cardano.Plutus.Test where

import PlutusTx.Prelude
import PlutusTx.Builtins
import PlutusTx.IsData
import Cardano.Api
import Plutus.V1.Ledger.Api
import Data.Aeson qualified as Aeson
import Cardano.Api.Shelley
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString qualified as Strict
import Data.Either.Extra

import Cardano.Plutus.OnChain.Bridge.NonceInteger

testNonceConstr = Lazy.fromStrict $ Strict.intercalate "\n" [
  testNonceDatum,
  testNonceNothingRedeemer,
  testNonceUpdateRedeemer
  ]

dataToJson :: ToData a => a -> Strict.ByteString
dataToJson = Lazy.toStrict . Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . builtinDataToData . toBuiltinData

jsonToData :: FromData a => Strict.ByteString -> Maybe a
jsonToData bs = do
    json <- Aeson.decode (Lazy.fromStrict bs)
    data' :: ScriptData <- eitherToMaybe $ scriptDataFromJson ScriptDataJsonDetailedSchema json
    fromBuiltinData $ dataToBuiltinData $ toPlutusData data'

testNonceNothingRedeemer = dataToJson (Nothing :: Maybe (Either NonceTransferRedeemer NonceRedeemer))
testNonceUpdateRedeemer = dataToJson (Just (Right $ NonceRedeemer 12 34) :: Maybe (Either NonceTransferRedeemer NonceRedeemer))
testNonceDatum = dataToJson (NonceDatum 123456)