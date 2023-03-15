{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.FromBS where

import Prelude

import Codec.Serialise
import Data.Semigroup
import Data.ByteString as Strict (ByteString)
import Cardano.Plutus.Validator
import Cardano.Plutus.Common
import Cardano.Plutus.ByteString
import Plutus.V1.Ledger.Scripts

import PlutusTx.Builtins
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value


class FromBS a where
    fromBS :: Strict.ByteString -> a

instance FromBS PubKeyHash where
    fromBS = PubKeyHash . toBuiltin

instance FromBS CurrencySymbol where
    fromBS = CurrencySymbol . toBuiltin

instance FromBS TokenName where
    fromBS = TokenName . toBuiltin

instance FromBS TxOutRef where
    fromBS bytes' =
        let bytes = toBuiltin bytes'
        in TxOutRef {
            txOutRefId = TxId $ sliceByteString 0 32 $ bytes,
            txOutRefIdx = bytesToInteger 256 $ sliceByteString 32 (lengthOfByteString bytes - 32) $ bytes
        }

instance FromBS ScriptHash where
    fromBS = ScriptHash . toBuiltin
