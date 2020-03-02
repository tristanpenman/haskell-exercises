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
