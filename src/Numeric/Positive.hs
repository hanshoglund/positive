
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Positive (
        Positive,
        positive,
        nonEmptyLength,
  ) where

import Numeric.Natural
import Data.Semigroup
import qualified Data.List.NonEmpty as NonEmpty

newtype Positive = Positive { getPositive :: Natural } deriving
    ( 
    Eq,
    Ord,
    -- Data,
    Real
    -- Ix,
    -- Typeable,
    -- Bits,
    -- Hashable,
    -- Whole
    )

instance Show Positive where
  show (Positive n) = show n

instance Read Positive where
  readsPrec d = map (\(n, s) -> (fromInteger n, s)) . readsPrec d

instance Integral Positive where
  toInteger (Positive a) = toInteger a
  Positive a `quotRem` Positive b = (fromIntegral $ a `quot` b, fromIntegral $ a `rem` b)
  Positive a `divMod` Positive b  = (fromIntegral $ a `div` b, fromIntegral $ a `mod` b)

instance Enum Positive where
  toEnum   = fromIntegral
  fromEnum = fromIntegral
  
instance Num Positive where
  Positive n + Positive m = Positive (n + m)
  Positive n * Positive m = Positive (n * m)
  Positive n - Positive m
    | r <  0 = error "Positive.(-): negative result"
    | r == 0 = error "Positive.(-): result zero"
    | otherwise   = Positive r
    where
      r = n - m
  abs      = id
  signum _ = 1
  fromInteger n
    | n >  0 = Positive (fromInteger n)
    | n == 0 = error "Positive.fromInteger: zero"
    | n <  0 = error "Positive.fromInteger: negative"

positive :: a -> (a -> a) -> Positive -> a
positive a f 1 = f a
positive a f n = f (positive a f $ pred n)

nonEmptyLength :: NonEmpty.NonEmpty a -> Positive
nonEmptyLength = fromIntegral . NonEmpty.length

-- _positive :: Integral a => Prism' a Positive
-- _positive :: prism' fromIntegral (\x -> if x > 0 then Just (fromInteger x :: Positive) else Nothing)

-- _nonEmpty :: Prism [a] [b] (NonEmpty a) (NonEmpty b)
-- _nonEmpty :: prism' NonEmpty.toList (\x -> if lenght x > 0 then Just (NonEmpty.fromList x) else Nothing)
