module Interval where

data Interval = Interval
  { start :: Int
  , end   :: Int
  } deriving (Show)

low :: Interval -> Int
low Interval {..} = min start end

high :: Interval -> Int
high Interval {..} = max start end

overlap :: Interval -> Interval -> Bool
overlap i1 i2 = max (low i1) (low i2) <= min (high i1) (high i2)

inside :: Int -> Interval -> Bool
inside x i = low i <= x && x <= high i
