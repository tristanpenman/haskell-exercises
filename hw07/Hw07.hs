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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ idx (Append _ jl1 jl2)
  | idx < jl1Size = indexJ idx jl1
  | otherwise     = indexJ (idx - jl1Size) jl2
    where jl1Size = getSize (size (tag jl1))
indexJ _ _ = Nothing

az :: JoinList Size Char
az = foldr1 (+++) $ Single (Size 1) <$> ['a'..'z']
