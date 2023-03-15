module Cardano.Plutus.ByteString where

import PlutusTx.Prelude
import PlutusTx.Builtins

{-# INLINABLE foldrByteString #-}
foldrByteString :: (Integer -> b -> b) -> b -> BuiltinByteString -> b
foldrByteString f start bytes = loop $ lengthOfByteString bytes
    where
        loop n = let m = n - 1 in f (indexByteString bytes m) (if m == 0 then start else loop m)

{-# INLINEABLE integerToDecBytes #-}
integerToDecBytes :: Integer -> BuiltinByteString
integerToDecBytes int = iterate int emptyByteString
    where
        base = 10
        iterate int
          | int < 0 = consByteString 45 . iterate (negate int)
          | quot < base = consByteString digit
          | otherwise = iterate quot . consByteString digit
            where
                quot = int `quotient` base
                rem = int `remainder` base
                digit = rem + 48

{-# INLINEABLE integerToBytes #-}
integerToBytes :: Integer -> Integer -> BuiltinByteString
integerToBytes base int = iterate int emptyByteString
  where
    iterate int = (if quot < base then id else iterate quot) . consByteString rem
        where
            quot = int `quotient` base
            rem = int `remainder` base

{-# INLINABLE bytesToInteger #-}
bytesToInteger :: Integer -> BuiltinByteString -> Integer
bytesToInteger base = foldrByteString (\cur acc -> acc * base + cur) 0