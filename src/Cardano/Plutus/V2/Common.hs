
module Cardano.Plutus.V2.Common where

import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts
import Cardano.Plutus.Common

{-# INLINABLE readRawDatum #-}
readRawDatum :: PlutusTx.FromData a => Datum -> Maybe a
readRawDatum = PlutusTx.fromBuiltinData . getDatum

-- Throws if couldn't read datum hash or parse datum
{-# INLINABLE readDatum #-}
readDatum :: PlutusTx.FromData a => TxInfo -> OutputDatum -> Maybe a
readDatum _ NoOutputDatum = Nothing
readDatum txIn (OutputDatumHash dh) = maybeTraceError "Failed to find hash of or decode datum" Just $ readRawDatum =<< flip findDatum txIn dh
readDatum _ (OutputDatum d) = maybeTraceError "Failed to find hash of or decode datum" Just $ readRawDatum d

readTxOutDatum :: PlutusTx.FromData a => TxInfo -> TxOut -> Maybe a
readTxOutDatum txIn TxOut{txOutAddress = Address{addressCredential=ScriptCredential _}, ..} =
    readDatum txIn txOutDatum
readTxOutDatum _ _ = traceError "Non-script output cannot have a datum"
