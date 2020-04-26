{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

--
-- Exercise 1
--
-- Define the function:
--
--     glCons :: Employee -> GuestList -> GuestList
--

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + (empFun e))

--
-- Part 2
--

instance Monoid GuestList where
  mempty  = GL [] 0

instance Semigroup GuestList where
  (GL es1 f1) <> (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

--
-- Part 3
--

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f2 > f1   = gl2
  | otherwise = gl1
