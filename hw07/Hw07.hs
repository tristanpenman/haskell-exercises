{-# OPTIONS_GHC -Wall #-}

module Hw07 where

import JoinList
import Sized

tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single m _) = m
tag (Append m _ _) = m

--
-- Exercise 1
--
-- Write an append function for JoinLists that yields a new JoinList whose monoidal annotation is
-- derived from those of the two arguments.
--
--   (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
--

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

--
-- Exercise 2
--
-- Part 1
--
-- Write the function:
--
--    indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
--

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ idx (Append _ jl1 jl2)
  | idx < jl1Size = indexJ idx jl1
  | otherwise     = indexJ (idx - jl1Size) jl2
    where jl1Size = getSize (size (tag jl1))
indexJ _ _ = Nothing

--
-- Part 2
--
-- Write the function:
--
--    dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
--

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl1@(Single _ _)
  | n < 1 = jl1
dropJ n (Append s jl1 jl2)
  | n < jl1Size = (dropJ n jl1) +++ jl2
  | n < totalSize = (dropJ (n - jl1Size) jl2)
    where jl1Size = getSize (size (tag jl1))
          totalSize = getSize (size s)
dropJ _ _ = Empty

--
-- Part 3
--
-- Write the function:
--
--     takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
--

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl1@(Single _ _)
    | n > 0 = jl1
takeJ n (Append _ jl1 jl2)
    | n >= jl1Size = jl1 +++ (takeJ (n - jl1Size) jl2)
      where jl1Size = getSize (size (tag jl1))
takeJ _ _ = Empty

-- Used for testing

az :: JoinList Size Char
az = foldr1 (+++) $ Single (Size 1) <$> ['a'..'z']
