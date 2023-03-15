{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cardano.Plutus.Common where

import Codec.Serialise
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, textEnvelopeRawCBOR, serialiseToTextEnvelope)
import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString.Short qualified as SBS
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString qualified as Strict
import Data.ByteString.Base16.Lazy as Lazy16
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)

import PlutusTx
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts

import Ledger.Interval qualified as Interval
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
import Ledger.Value
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test hiding (not)
import PlutusTx qualified
import PlutusTx.Prelude hiding (fold, foldr)
import PlutusTx.Prelude qualified as PP
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins hiding (foldr)
import PlutusTx.Foldable (foldl, foldr)
import PlutusTx.Numeric ()
import PlutusTx.Maybe (maybe)

import Prelude qualified as Haskell
import Schema (ToSchema)
import Control.Arrow ((&&&))

{-# INLINABLE first3M #-}
first3M :: Functor f => (a -> f a') -> (a,b,c) -> f (a',b,c)
first3M f (a,b,c) = fmap (,b,c) $ f a

{-# INLINABLE second3M #-}
second3M :: Functor f => (b -> f b') -> (a,b,c) -> f (a,b',c)
second3M f (a,b,c) = fmap (a,,c) $ f b

{-# INLINABLE third3M #-}
third3M :: Functor f => (c -> f c') -> (a,b,c) -> f (a,b,c')
third3M f (a,b,c) = fmap (a,b,) $ f c

{-# INLINABLE foldMap_ #-}
foldMap_ :: Monoid m => (a -> m) -> [a] -> m
foldMap_ _ []     = mempty
foldMap_ f (x:xs) = f x <> foldMap_ f xs

{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a

{-# INLINABLE outIdxHash #-}
outIdxHash :: Tx.TxOutRef -> BuiltinByteString
outIdxHash Tx.TxOutRef {..} = consByteString txOutRefIdx $ sliceByteString 0 31 $ getTxId txOutRefId

{-# INLINABLE outTxId #-}
outTxId :: Tx.TxOutRef -> BuiltinByteString
outTxId = getTxId . txOutRefId

{-# INLINABLE biliftA2 #-}
biliftA2 :: (a -> b -> c) -> (a' -> b' -> c') -> (a, a') -> (b, b') -> (c, c')
h `biliftA2` h' = \ (a, a') (b, b') -> (h a b, h' a' b')

{-# INLINABLE on #-}
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x -> let fx = f x in \y -> fx .*. f y

{-# INLINABLE groupOn #-}
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = map snd . foldr go [] . map (f &&& id)
    where
        go (k, x) ((k', xs) : ys) | k == k' = (k, x:xs) : ys
        go (k, x) kxs = (k, [x]) : kxs

{-# INLINABLE maybeOnly #-}
maybeOnly :: [a] -> Maybe a
maybeOnly [a] = Just a
maybeOnly _ = Nothing

{-# INLINABLE ensure #-}
ensure :: (a -> Bool) -> a -> Maybe a
ensure p a
  | p a       = Just a
  | otherwise = Nothing

{-# INLINABLE foldl1 #-}
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
foldl1 f xs = fromMaybeTraceError "foldl1: empty structure" (foldl mf Nothing xs)
    where
        mf m y = Just $ maybe y (`f` y) m

{-# INLINABLE allOf #-}
allOf :: (b -> a -> b) -> b -> [a] -> Maybe b
allOf _ _ [] = Nothing
allOf f a as = Just $ foldl f a as

{-# INLINABLE swap #-}
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

{-# INLINABLE if' #-}
if' :: Bool -> a -> a -> a
if' cond true false = if cond then true else false

{-# INLINABLE flap #-}
flap :: Functor f => f (a -> b) -> a -> f b
flap f x = fmap ($ x) f

infixl 4 <@>
{-# INLINABLE (<@>) #-}
(<@>) :: Functor f => f (a -> b) -> a -> f b
(<@>) = flap

{-# INLINABLE valueOf' #-}
valueOf' p a v = Value.valueOf v p a

{-# INLINABLE onlyValueOf #-}
onlyValueOf symbol token = Value.singleton symbol token . valueOf' symbol token

{-# INLINABLE zeroValue #-}
zeroValue = Value.singleton adaSymbol adaToken 0

{-# INLINABLE emptyTokenName #-}
emptyTokenName = TokenName emptyByteString

{-# INLINABLE maybeTraceError #-}
maybeTraceError str _ Nothing = traceError str
maybeTraceError _ f (Just a) = f a

{-# INLINABLE fromMaybeTraceError #-}
fromMaybeTraceError str Nothing = traceError str
fromMaybeTraceError _ (Just a) = a

{-# INLINABLE valueAssetsQty #-}
-- Including ADA
valueAssetsQty :: Value -> Integer
valueAssetsQty = length . flattenValue


{-# INLINABLE hasCurrency #-}
hasCurrency :: CurrencySymbol -> Value -> Bool
hasCurrency symbol = isJust . PMap.lookup symbol . getValue

{-# INLINABLE findCurrency #-}
findCurrency :: CurrencySymbol -> Value -> Maybe (PMap.Map TokenName Integer)
findCurrency symbol = PMap.lookup symbol . getValue


-- Spent or referenced from signing PubKeyHash
{-# INLINABLE spentOrReferencedValue #-}
spentOrReferencedValue txIn pred = if spent then True else referenced
    where
        spent = (pred . txOutValue . txInInfoResolved) `any` txInfoInputs txIn
        referenced = (witnessed . txInInfoResolved) `any` txInfoReferenceInputs txIn
        witnessed txOut = if pred $ txOutValue $ txOut
            then txSignedByAddress $ txOutAddress txOut
            else False
        txSignedByAddress Address { addressCredential = PubKeyCredential hash } = txSignedBy txIn hash
        txSignedByAddress _ = False
