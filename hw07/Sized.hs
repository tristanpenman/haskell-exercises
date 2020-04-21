{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Sized where

import Data.Monoid
import Data.Semigroup

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

instance Monoid Size where
  mempty  = 0
  mappend = (+)

instance Semigroup Size where
  (Size m1) <> (Size m2) = Size (m1 + m2)
