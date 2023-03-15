
module Cardano.Plutus.V1.Contexts where

import PlutusTx.Prelude

import Cardano.Plutus.Common
import Cardano.Plutus.V1.Common
import Cardano.Plutus.List
import Data.Tuple.Extra hiding (fst, snd)
import PlutusTx
import PlutusTx.Foldable
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Scripts

import PlutusTx.AssocMap qualified as PMap

{-# INLINABLE consumedUTxO #-}
consumedUTxO :: (TxInInfo -> Bool) -> TxInfo -> Bool
consumedUTxO pred txIn = pred `any` txInfoInputs txIn -- length (inputsThat pred txIn) > 0

{-# INLINABLE scriptInputsAt #-}
scriptInputsAt :: ValidatorHash -> TxInfo -> [(DatumHash, Value)]
scriptInputsAt h p =
    let flt TxOut{txOutDatumHash=Just ds, txOutAddress=Address (ScriptCredential s) _, txOutValue} | s == h = Just (ds, txOutValue)
        flt _ = Nothing
    in mapMaybe flt (txInInfoResolved `map` txInfoInputs p)

{-# INLINABLE scriptOutputsThat #-}
scriptOutputsThat :: TxInfo -> (ValidatorHash -> Bool) -> [(ValidatorHash, Value, DatumHash)]
scriptOutputsThat TxInfo {..} pred =
    [(hash, txOutValue, datumhash) | TxOut{txOutDatumHash=Just datumhash, txOutAddress=Address (ScriptCredential hash) _, txOutValue} <- txInfoOutputs, pred hash ]

{-# INLINABLE scriptOutputs #-}
scriptOutputs :: TxInfo -> [(ValidatorHash, Value, DatumHash)]
scriptOutputs txIn = scriptOutputsThat txIn (const True)

{-# INLINABLE scriptTxInsThat #-}
scriptTxInsThat :: TxInfo -> (ValidatorHash -> Bool) -> [TxInInfo]
scriptTxInsThat TxInfo {..} pred =
    [ txIn | txIn@TxInInfo{txInInfoResolved = TxOut{txOutDatumHash=Just datumhash, txOutAddress=Address (ScriptCredential hash) _, txOutValue}}
        <- txInfoInputs, pred hash ]

{-# INLINABLE valueSpentFromScript #-}
-- | Get the total value spent by the given validator in this transaction.
valueSpentFromScript :: ValidatorHash -> TxInfo -> Value
valueSpentFromScript h ptx =
    let outputs = map snd (scriptInputsAt h ptx)
    in mconcat outputs

{-# INLINABLE hasOnlyDatumAt #-}
hasOnlyDatumAt :: FromData a => TxInfo -> (a -> Bool) -> ValidatorHash -> Bool
hasOnlyDatumAt txIn pred hash = or . fmap pred $ (readDatum txIn . thd3) =<<
    onlyAndSatisfies ((hash ==) . fst3) (scriptOutputs txIn)

{-# INLINABLE hasOnlyScriptOut #-}
hasOnlyScriptOut :: FromData a => TxInfo -> ((ValidatorHash, Value, a) -> Bool) -> Bool
hasOnlyScriptOut txIn pred = isJust $ onlyAndSatisfies (or . fmap pred . third3M (readDatum txIn)) (scriptOutputs txIn)

{-# INLINABLE publicKeyOutputs #-}
publicKeyOutputs :: TxInfo -> [(PubKeyHash, Value)]
publicKeyOutputs TxInfo {..} =
    [(hash, txOutValue) | TxOut{txOutAddress=Address (PubKeyCredential hash) _, txOutValue} <- txInfoOutputs ]

{-# INLINABLE onlyPaidTo #-}
onlyPaidTo :: TxInfo -> PubKeyHash -> Bool
onlyPaidTo txIn hash = valuePaidTo txIn hash == foldMap snd (publicKeyOutputs txIn)

{-# INLINABLE getOwnTokens #-}
getOwnTokens :: ScriptContext -> Value -> [(TokenName, Integer)]
getOwnTokens ctx = concat . fmap PMap.toList . PMap.lookup (ownCurrencySymbol ctx) . getValue