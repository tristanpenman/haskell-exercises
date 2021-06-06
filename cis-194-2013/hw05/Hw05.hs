{-# OPTIONS_GHC -Wall #-}

module Hw05 where

import ExprT
import Parser

--
-- Exercise 1
--

eval :: ExprT -> Integer
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b
eval (Lit v) = v

--
-- Exercise 2
--

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--
-- Exercise 3
--

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

--
-- Exercise 4
--
-- Make instances of Expr for the following types:
--
--   Integer - Works like the original calculator
--
--   Bool    - Every literal value <= 0 is interpreted as false, and all
--             positive integers are interpreted as false. Addition is treated
--             as logical OR; multiplication is logical AND
--
--   MinMax  - addition is taken to be the 'max' function, while multiplication
--             is the 'min' function
--
--   Mod7    - all arithmetic should be done modulo 7
--

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

--
-- Integer
--

instance Expr Integer where
  lit a = a
  add a b = a + b
  mul a b = a * b

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

--
-- Bool
--

instance Expr Bool where
  lit a = a > 0
  add a b = a || b
  mul a b = a && b

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

--
-- MinMax
--

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

--
-- Mod7
--

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7
