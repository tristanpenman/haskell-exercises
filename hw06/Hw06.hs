{-# OPTIONS_GHC -Wall #-}

module Hw06 where

--
-- Exercise 1
--
-- Come up with
--

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--
-- Exercise 2
--
-- Now come up with more a efficient implementation.
--

fibs2 :: [Integer]
fibs2 = let fibs2' a b = a : (fibs2' b (a + b))
        in fibs2' 0 1

fibs3 :: [Integer]
fibs3 = let fibs3' a b = a : (fibs3' b $ a + b)
        in fibs3' 0 1

--
-- Exercise 3
--
-- Define a data type of polymorphic streams, Stream.
--
-- Then write a function to convert a Stream to an infinite list:
--

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

-- TODO: This can be improved
instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

--
-- Exercise 4
--

--
-- Part 1
--
-- Write a function:
--
--    streamRepeat :: a -> Stream a
--
-- which generates a stream containing infinitely many copies of the
-- given element
--

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

--
-- Part 2
--
-- Write a function
--
--    streamMap :: (a -> b) -> Stream a -> Stream b
--
-- which applies a function to every element of a Stream.
--

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Stream s xs) = Stream (fn s) (streamMap fn xs)

--
-- Part 3
--
-- Write a function
--
--    streamFromSeed :: (a -> a) -> a -> Stream a
--
-- which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a
--

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn s = Stream s (streamFromSeed fn (fn s))

--
-- Exercise 5
--

--
-- Part 1 - natural numbers
--

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

--
-- Part 2 - ruler function
--
-- Hint: define a function interleaveStreams which alternates the elements
-- from two streams. Canyou use this function to implement 'ruler' in a
-- clever way that does not have to do any divisibility testing?
--

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Stream x xs) ys = Stream x (streamInterleave ys xs)

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)
