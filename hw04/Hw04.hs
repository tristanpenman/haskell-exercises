{-# OPTIONS_GHC -Wall #-}

module Hw04 where

--
-- Exercise 1 - Wholemeal programming
--
-- Part 1
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
fun1' = foldr (*) 1 . map ((-) 2) . filter even

--
-- Part 2
--
-- Hint: For this problem you may wish to use the functions iterate and
-- takeWhile. Look them up in the Prelude documentation to see what they do.
--

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

--
-- Expanding out for an example:
--
-- fun2 3
--     = fun2 (3 * 3 + 1)
--     = fun2 10
--     = 10 + fun2 5
--     = 10 + fun2 (3 * 5 + 1)
--     = 10 + fun2 16
--     = 10 + (16 + fun2 8)
--     = 10 + (16 + (8 + fun2 4))
--     = 10 + (16 + (8 + (4 + fun 2)))
--     = 10 + (16 + (8 + (4 + 2 + fun 1)))
--     = 10 + (16 + (8 + (4 + 2 + 0)))
--     = 40
--
-- Notes: A value for `n` is only added to the sum when it is even; odd numbers 'fall through',
-- but become even... since `iterate` will return all numbers, we can filter them out.
--
-- First, try combining `takeWhile` and `iterate`:
--

fun2'1 :: Integer -> [Integer]
fun2'1 = takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

--
-- fun2'1 3 = [3,10,5,16,8,4,2]
--
-- (sum . filter even . fun2'1) 3 = 40
--

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

--
-- Exercise 2 - Folding with trees
--
-- Given a definition of a tree...
--

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

--
-- define a function which generates a balanced binary tree from a list of
-- values using foldr. The does not need to have a particular order, but
-- it must be balanced.
--

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = (Node 0 Leaf x Leaf)
insert x (Node _ left val right)
    -- check whether right subtree needs some love
    | rightHt <= leftHt = let newRight = insert x right
                              newRightHt = height newRight
                          in Node (succ $ max leftHt $ newRightHt) left val newRight
    -- otherwise default to left subtree
    | otherwise         = let newLeft = insert x left
                              newLeftHt = height newLeft
                          in Node (succ $ max newLeftHt $ rightHt) newLeft val right
  where leftHt = height left
        rightHt = height right
