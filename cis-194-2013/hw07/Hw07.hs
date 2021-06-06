{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Hw07 where

import Buffer
import Editor
import JoinList
import Scrabble
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
(+++) a Empty = a
(+++) Empty b = b
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

--
-- Exercise 3
--
-- See Scrabble.hs
--

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

--
-- Exercise 4
--
-- Provide a Buffer instance for the type:
--
--    JoinList (Score, Size) String
--
-- For reference, this is the Buffer instance for the String type:
--
--    instance Buffer String where
--      toString     = id
--      fromString   = id
--      line n b     = safeIndex n (lines b)
--      replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
--        where replaceLine' pre [] = pre
--              replaceLine' pre (_:ls) = pre ++ l:ls
--      numLines     = length . lines
--      value        = length . words
--

instance Buffer (JoinList (Score, Size) String) where
  -- Simple recursive conversion
  toString Empty = ""
  toString (Single _ s) = s ++ "\n"
  toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2

  -- Single entries represent lines in the data structure,
  -- so first split into lines, and for each line calculate the score and set size to 1
  fromString s = foldr (+++) Empty (map (\x -> Single (scoreString x, (Size 1)) x) (lines s))

  -- Fetch a particular line... ez
  line = indexJ

  -- Take all the lines before the line being replaced, append the new line, then take all of the
  -- lines after the line being replaced
  replaceLine n s b
    | n < numLines b = (takeJ n b) +++ fromString s +++ (dropJ (n + 1) b)
    | otherwise      = b

  -- Extract tags
  numLines = getSize . snd . tag
  value    = getScore . fst . tag

main :: IO()
main = runEditor editor $ Single ((Score 0), (Size 0)) ""
