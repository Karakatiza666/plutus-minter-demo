{-# LANGUAGE FlexibleContexts #-}

module Cardano.Plutus.List where

import PlutusTx.Prelude
import Data.Align
import Data.These

{-# INLINABLE onlyNAndSatisfy #-}
onlyNAndSatisfy :: Integer -> (a -> Bool) -> [a] -> Maybe [a]
onlyNAndSatisfy n pred list = if length list == n then if pred `all` list then Just list else Nothing else Nothing

{-# INLINABLE onlyAndSatisfies #-}
onlyAndSatisfies :: (a -> Bool) -> [a] -> Maybe a
onlyAndSatisfies pred list = let h = head list in if length list == 1 then if pred h then Just h else Nothing else Nothing

{-# INLINABLE only #-}
only :: [a] -> Maybe a
only [a] = Just a
only _ = Nothing

{-# INLINABLE zipWithDefault #-}
zipWithDefault :: Align f => a -> b -> f a -> f b -> f (a, b)
zipWithDefault da db = alignWith (fromThese da db)

-- Should only be applied to sorted lists
-- Pads both lists with default values so that pairwise element's keys are equal
{-# INLINABLE unzipMatchDefault #-}
unzipMatchDefault :: Ord k => (a -> k) -> (k -> a) -> [a] -> [a] -> ([a], [a])
unzipMatchDefault key dx = match
    where
        dy = dx
        match xs         [] = (xs, (dy . key) `map` xs)
        match []         ys = ((dx . key) `map` ys, ys)
        match ax@(x:xs) ay@(y:ys) = case compare (key x) (key y) of
            EQ -> ($ match xs ys ) \ (as, bs) -> ( x:as,                 y:bs)
            LT -> ($ match xs ay ) \ (as, bs) -> ( x:as,      (dy $ key x):bs)
            GT -> ($ match ax ys ) \ (as, bs) -> ((dx $ key y):as,       y:bs)

{-# INLINABLE zipWithMatchDefault #-}
zipWithMatchDefault :: Ord k => (a -> k) -> (a -> a -> b) -> (k -> a) -> [a] -> [a] -> [b]
zipWithMatchDefault key f dx = match
    where
        dy = dx
        match xs        [] = map (\x -> f x (dy $ key x)) xs -- (ap f (dy . key)) -- (\x -> (x `f` dy (key x)))
        match []        ys = map (f =<< dx . key) ys -- (\y -> dx (key y) `f` y)
        match ax@(x:xs) ay@(y:ys) = case compare (key x) (key y) of
            EQ -> f x             y : match xs ys
            LT -> f x  (dy $ key x) : match xs ay
            GT -> f (dx $ key y)  y : match ax ys

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- >>> span (< 3) [1,2,3,4,1,2,3,4]
-- ([1,2],[3,4,1,2,3,4])
-- >>> span (< 9) [1,2,3]
-- ([1,2,3],[])
-- >>> span (< 0) [1,2,3]
-- ([],[1,2,3])
--
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
{-# INLINABLE span #-}
span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)