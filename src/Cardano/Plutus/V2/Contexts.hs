{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Plutus.V2.Contexts where

import Cardano.Plutus.Common
import Cardano.Plutus.Sort
import Cardano.Plutus.V2.Common

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)

import Ledger.Ada qualified as Ada
import Ledger.Address
import Ledger.Credential (Credential(..))
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain ()
import Ledger.Interval qualified as Interval
import Ledger.Value (Value)
import Ledger.Value 
import Ledger.TimeSlot qualified as TimeSlot

import Playground.Contract


import Cardano.Plutus.List
import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts

import PlutusTx.AssocMap qualified as PMap

import Prelude qualified as Haskell
import Schema (ToSchema)
import Control.Arrow ((&&&))

{-# INLINABLE consumedUtxoForName #-}
consumedUtxoForName outRefId tokenName =
    consumedUTxO $ (unTokenName tokenName ==) . outRefId . txInInfoOutRef

{-# INLINABLE consumedUTxO #-}
consumedUTxO :: (TxInInfo -> Bool) -> TxInfo -> Bool
consumedUTxO pred txIn = length (inputsThat pred txIn) > 0

{-# INLINABLE spentTxOutRef #-}
spentTxOutRef :: TxOutRef -> TxInfo -> Bool
spentTxOutRef ref txIn = ((ref ==) . txInInfoOutRef) `any` txInfoInputs txIn

{-# INLINABLE txOutsSpentFromScript #-}
txOutsSpentFromScript :: TxInfo -> ValidatorHash -> [TxOut]
txOutsSpentFromScript TxInfo {..} hash =
    filter (\ TxOut{..} -> (Just hash ==) $ toValidatorHash $ txOutAddress ) $ map txInInfoResolved txInfoInputs

{-# INLINABLE txOutsSpentFrom #-}
txOutsSpentFrom :: TxInfo -> Credential -> [TxOut]
txOutsSpentFrom TxInfo {..} cred =
    filter (\ TxOut{..} -> cred == addressCredential txOutAddress ) $ map txInInfoResolved txInfoInputs

{-# INLINABLE valueSpentFromScript' #-}
valueSpentFromScript' :: TxInfo -> ValidatorHash -> Maybe Value
valueSpentFromScript' txIn = allOf (<>) mempty . fmap txOutValue . txOutsSpentFromScript txIn

{-# INLINABLE valueSpentFrom #-}
valueSpentFrom :: TxInfo -> Credential -> Maybe Value
valueSpentFrom txIn = allOf (<>) mempty . fmap txOutValue . txOutsSpentFrom txIn

{-# INLINABLE publicKeyOutputs #-}
publicKeyOutputs :: TxInfo -> [(PubKeyHash, Value)]
publicKeyOutputs TxInfo {..} =
    [(hash, txOutValue) | TxOut{txOutAddress=Address (PubKeyCredential hash) _, txOutValue} <- txInfoOutputs ]

{-# INLINABLE scriptOutputsThat #-}
scriptOutputsThat :: (TxOut -> Bool) -> TxInfo -> [TxOut]
scriptOutputsThat pred TxInfo {..} =
    [txOut | txOut@TxOut{txOutAddress=Address (ScriptCredential _) _} <- txInfoOutputs, pred txOut ]

-- Defined in Plutus.V2.Ledger.Contexts
{-# INLINABLE scriptOutputsAt #-}
scriptOutputsAt :: ValidatorHash -> TxInfo -> [TxOut]
scriptOutputsAt = scriptOutputsThat . atScriptAddress

{-# INLINABLE scriptOutputs #-}
scriptOutputs :: TxInfo -> [TxOut]
scriptOutputs = scriptOutputsThat (const True)

{-# INLINABLE inputsThat #-}
inputsThat :: (TxInInfo -> Bool) -> TxInfo -> [TxOut]
inputsThat pred txIn = [ txInInfoResolved i | i <- txInfoInputs txIn, pred i ]

{-# INLINABLE scriptInputsThat #-}
scriptInputsThat :: (TxOut -> Bool) -> TxInfo -> [TxOut]
scriptInputsThat pred TxInfo {..} =
    [txOut | TxInInfo { txInInfoResolved = txOut@TxOut{txOutAddress=Address (ScriptCredential _) _} } <- txInfoInputs, pred txOut ]

{-# INLINABLE scriptInputsAt #-}
scriptInputsAt :: ValidatorHash -> TxInfo -> [TxOut]
scriptInputsAt = scriptInputsThat . atScriptAddress

{-# INLINABLE scriptInputs #-}
scriptInputs :: TxInfo -> [TxOut]
scriptInputs = scriptInputsThat (const True)

{-# INLINABLE spentScriptInput #-}
spentScriptInput :: ValidatorHash -> TxInfo -> Bool
spentScriptInput h txIn = length (scriptInputsAt h txIn) > 0

{-# INLINABLE valueSpentFromScript #-}
-- | Get the total value spent by the given validator in this transaction.
valueSpentFromScript :: ValidatorHash -> TxInfo -> Value
valueSpentFromScript h ptx =
    let outputs = map txOutValue $ scriptInputsAt h ptx
    in mconcat outputs

{-# INLINABLE atScriptAddress #-}
atScriptAddress :: ValidatorHash -> TxOut -> Bool
atScriptAddress hash = (ScriptCredential hash ==) . addressCredential . txOutAddress

{-# INLINABLE thatScriptAddress #-}
thatScriptAddress :: (ValidatorHash -> Bool) -> TxOut -> Bool
thatScriptAddress pred =
    (\case
        ScriptCredential hash -> pred hash
        _ -> False
    ) . addressCredential . txOutAddress

-- does given script have a single output with specified datum
{-# INLINABLE hasOnlyDatumAt #-}
hasOnlyDatumAt :: PlutusTx.FromData a => TxInfo -> (a -> Bool) -> ValidatorHash -> Bool
hasOnlyDatumAt txIn pred hash = or . fmap pred $ (readDatum txIn . txOutDatum) =<<
    only (scriptOutputsThat (atScriptAddress hash) txIn)

-- does given script have a single output with the same datum as current UTxO - so datum is continuing
{-# INLINABLE hasOnlyOwnDatum #-}
hasOnlyOwnDatum :: ScriptContext -> Bool
hasOnlyOwnDatum ctx@ScriptContext{scriptContextTxInfo=txIn} =
    isJust $ only (scriptOutputsThat ((ownDatum ==) . txOutDatum) txIn)
    where
        ownDatum = txOutDatum $ txInInfoResolved $ fromMaybeTraceError "not script" $ findOwnInput ctx

-- Has a single output of given script that satisfies pred
{-# INLINABLE hasOnlyScriptOutThat #-}
hasOnlyScriptOutThat :: ValidatorHash -> (TxOut -> Bool) -> TxInfo -> Bool
hasOnlyScriptOutThat hash pred txIn = isJust $ onlyAndSatisfies pred (Cardano.Plutus.V2.Contexts.scriptOutputsAt hash txIn)

-- Has a single script input that satisfies pred
{-# INLINABLE hasOnlyScriptIn #-}
hasOnlyScriptIn :: (TxOut -> Bool) -> TxInfo -> Bool
hasOnlyScriptIn pred = isJust . onlyAndSatisfies pred . scriptInputs

{-# INLINABLE hasOnlyScriptInAt #-}
hasOnlyScriptInAt :: ValidatorHash -> TxInfo -> Bool
hasOnlyScriptInAt hash = hasOnlyScriptIn (atScriptAddress hash)

{-# INLINABLE onlyPaidTo #-}
onlyPaidTo :: TxInfo -> PubKeyHash -> Bool
onlyPaidTo txIn hash = valuePaidTo txIn hash == foldMap snd (publicKeyOutputs txIn)

{-# INLINABLE getPubKeyOutValues #-}
getPubKeyOutValues :: TxInfo -> [(PubKeyHash, Value)]
getPubKeyOutValues TxInfo { .. } =
    let pubKeyOuts = mapMaybe (\TxOut {..} -> (, txOutValue) <$> toPubKeyHash txOutAddress) txInfoOutputs
    in map (foldl1 $ biliftA2 const (<>)) $ groupOn fst pubKeyOuts

{-# INLINABLE valueTransferredTo #-}
valueTransferredTo :: ValidatorHash -> TxInfo -> Value
valueTransferredTo script txIn = valueLockedBy txIn script - valueSpentFromScript script txIn

{-# INLINABLE assetTransferredTo #-}
assetTransferredTo :: CurrencySymbol -> TokenName -> ValidatorHash -> TxInfo -> Integer
assetTransferredTo currency token = (valueOf' currency token . ) . valueTransferredTo

{-# INLINABLE aclTransferredTo #-}
aclTransferredTo :: AssetClass -> ValidatorHash -> TxInfo -> Integer
aclTransferredTo (AssetClass (currency, token)) = (valueOf' currency token . ) . valueTransferredTo

inlineDatum :: FromData a => OutputDatum -> Maybe a
inlineDatum (OutputDatum d) = readRawDatum d
inlineDatum _ = Nothing

outputDatums :: FromData a => ValidatorHash -> TxInfo -> [Maybe a]
outputDatums hash txIn = map (readTxOutDatum txIn) $ scriptOutputsThat (atScriptAddress hash) txIn

requireOutputDatums :: FromData a => ValidatorHash -> TxInfo -> [a]
requireOutputDatums hash txIn = map
    (fromMaybeTraceError "Datum not present in atleast some of UTxOs" . readTxOutDatum txIn)
    (scriptOutputsThat (atScriptAddress hash) txIn)

{-# INLINABLE getOwnTokens #-}
getOwnTokens :: ScriptContext -> Value -> [(TokenName, Integer)]
getOwnTokens ctx = concat . fmap PMap.toList . PMap.lookup (ownCurrencySymbol ctx) . getValue

{-# INLINABLE referenceAt #-}
referenceAt :: TxOutRef -> TxInfo -> Maybe TxOut
referenceAt ref txIn = txInInfoResolved <$> find ((ref ==) . txInInfoOutRef) (txInfoReferenceInputs txIn)

instance Ord Credential

{-# INLINABLE addressesValue #-}
addressesValue :: [TxOut] -> [(Credential, Value)]
addressesValue = monoidSortAssocsWith (addressCredential . txOutAddress) txOutValue

{-# INLINABLE addressesSpentFilter #-}
addressesSpentFilter :: (Credential -> Bool) -> TxInfo -> [(Credential, Value)]
addressesSpentFilter pred = addressesValue . filter (pred . addressCredential . txOutAddress) . map txInInfoResolved . txInfoInputs

{-# INLINABLE addressesPaidFilter #-}
addressesPaidFilter :: TxInfo -> [(Credential, Value)]
addressesPaidFilter = addressesValue . txInfoOutputs

{-# INLINABLE requireOwnInput #-}
requireOwnInput :: ScriptContext -> TxInInfo
requireOwnInput = fromJust . findOwnInput