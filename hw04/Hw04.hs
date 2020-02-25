{-# OPTIONS_GHC -Wall #-}

module Hw04 where

--
-- Exercise 1 - Wholemeal programming
--

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

--
-- Expanding this out for some examples:
--
-- fun1 [1,2,3,4]
--     = fun1 [2,3,4]
--     = (0) * fun [3,4] ...
--     = 0

-- fun1 [1,4,3,8]
--     = fun1 [4,3,8]
--     = (2) * fun [3,8]
--     = (2) * fun1 8
--     = (2) * (6) * fun1 []
--     = 12
--
-- TODO: multiplication is commutative, so it makes sense to me that foldl and
-- foldr should be equivalent. Need to verify this.
--

fun1'1 :: [Integer] -> Integer
fun1'1 xs = foldr fn 1 xs
  where fn x acc
          | even x = (x - 2) * acc
          | otherwise = acc

fun1'2 :: [Integer] -> Integer
fun1'2 xs = foldr (*) 1 values
  where values = map (\x -> if even x then (x - 2) else 1) xs

fun1'3 :: [Integer] -> Integer
fun1'3 xs = foldr (*) 1 (map (\x -> x - 2) (filter even xs))

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even
