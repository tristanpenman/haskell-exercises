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
