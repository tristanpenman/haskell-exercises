{-# OPTIONS_GHC -Wall #-}

module Golf where

--
-- Exercise 1 - Hopscotch
--
-- The first list in the output should be the same as the input list. The
-- second list in the output should contain every second element from the
-- input list... and the nth list in the output should contain every nth
-- element from the input list.
--
-- Examples:
--
--     skips "ABCD"       == ["ABCD", "BD", "C", "D"]
--     skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
--     skips [1]          == [[1]]
--     skips [True,False] == [[True,False], [False]]
--     skips []           == []
--

skips :: [a] -> [[a]]
skips xs = [pickEveryNth xs i | i <- [1..length xs] ]

pickEveryNth :: [a] -> Int -> [a]
pickEveryNth xs n = pick 1 n xs
  where pick :: Int -> Int -> [a] -> [a]
        pick _ _ [] = []
        pick i c (y:ys)
          | i == c    = y : pick 1 c ys
          | otherwise = pick (i + 1) c ys

--
-- Found a clever alternative to pickEveryNth on Github:
-- https://github.com/bschwb/cis194-solutions/blob/master/03-rec-poly/Golf.hs
--
-- Returns each @n@-th element of the list.
--
-- Using list comprehension. Index list returns exactly the indices of every
-- n-th element. Is safe because when the @lst@ is empty then the index list is
-- also empty. And indices go from 0 to (length lst) - 1, so also we don't
-- index anything to big.
--
-- > pickEveryNth2 2 [1, 2, 3, 4] = [2, 4]
-- > pickEveryNth2 2 [] = []
--
pickEveryNth2 :: Int -> [a] -> [a]
pickEveryNth2 n lst = [lst !! i | i <- [n-1, n-1+n..length lst - 1]]

skips2 :: [a] -> [[a]]
skips2 xs = [pickEveryNth2 i xs | i <- [1..length xs] ]

--
-- Exercise 2 - Local maxima
--
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, since it is
-- greater than the elements immediately before and after it (3 and 1).
-- 5 is not a local maximum since there is no element that comes after it.
--
-- Examples:
--
--     localMaxima [2,9,5,6,1] == [9,6]
--     localMaxima [2,3,4,1,5] == [4]
--     localMaxima [1,2,3,4,5] == []
--

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | x < y && y > z = y : localMaxima (y:z:xs)
  | otherwise      = localMaxima (y:z:xs)
localMaxima _ = []
