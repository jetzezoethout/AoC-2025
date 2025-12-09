module Interval where

data Orientation
  = Positive
  | Negative
  deriving (Show)

opposite :: Orientation -> Orientation -> Bool
opposite Positive Negative = True
opposite Negative Positive = True
opposite _ _               = False

data Interval = Interval
  { low         :: Int
  , high        :: Int
  , orientation :: Orientation
  } deriving (Show)

makeInterval :: Int -> Int -> Interval
makeInterval start end =
  Interval
    { low = min start end
    , high = max start end
    , orientation =
        if start < end
          then Positive
          else Negative
    }

overlap :: Interval -> Interval -> Bool
overlap i1 i2 = max i1.low i2.low < min i1.high i2.high

collide :: Interval -> Interval -> Bool
collide i1 i2 = opposite i1.orientation i2.orientation && overlap i1 i2
