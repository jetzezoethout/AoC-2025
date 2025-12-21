{-# LANGUAGE DerivingStrategies #-}

module LinAlg.Vector where

import           Data.Ratio ((%))

newtype Scalar =
  Scalar Rational
  deriving newtype (Eq, Ord, Num, Fractional)
  deriving stock (Show)

toScalar :: Integral a => a -> Scalar
toScalar n = Scalar $ fromIntegral n % 1

infixl 6 ^+
infixl 6 ^-
infixl 7 ^*
class Eq v =>
      Vector v
  where
  (^+) :: v -> v -> v
  (^*) :: Scalar -> v -> v

(^-) :: Vector v => v -> v -> v
x ^- y = x ^+ ((-1) ^* y)

sumVecs :: Vector v => [v] -> v
sumVecs = foldr1 (^+)

instance Vector Scalar where
  (^+) :: Scalar -> Scalar -> Scalar
  l ^+ m = l + m
  (^*) :: Scalar -> Scalar -> Scalar
  l ^* m = l * m

instance Vector v => Vector [v] where
  (^+) :: Vector v => [v] -> [v] -> [v]
  (^+) = zipWith (^+)
  (^*) :: Vector v => Scalar -> [v] -> [v]
  (^*) l = map (l ^*)
