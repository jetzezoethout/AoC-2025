module Interval where

data Interval = Interval
  { low  :: Int
  , high :: Int
  } deriving (Show)

makeInterval :: Int -> Int -> Interval
makeInterval start end = Interval {low = min start end, high = max start end}

overlapStrictly :: Interval -> Interval -> Bool
overlapStrictly i1 i2 = max i1.low i2.low < min i1.high i2.high

strictlyInside :: Int -> Interval -> Bool
strictlyInside x Interval {..} = low < x && x < high
