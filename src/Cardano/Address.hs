
module Cardano.Address where

import Prelude
import Codec.Binary.Bech32
import Data.ByteString.Lazy
import Data.Either
import Data.Maybe
import Data.Text.Encoding
import Data.ByteString.Base16.Lazy qualified as B16

extractPubKeyHash :: ByteString -> ByteString
extractPubKeyHash =
    fromStrict . fromMaybe (error "Failed to extract bytes of bech32 address") . dataPartToBytes . snd
  . fromRight (error "Failed to decode bech32 address") . decode . decodeUtf8 . toStrict

extractTokenName :: ByteString -> ByteString
extractTokenName = fromRight (error "Failed to decode hex tokenName") . B16.decode