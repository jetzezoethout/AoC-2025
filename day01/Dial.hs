module Dial where

import           DialRotation (DialDirection (..), DialRotation (..))

data Dial = Dial
  { position     :: Int
  , zeroesPassed :: Int
  } deriving (Show)

initialDial :: Dial
initialDial = Dial {position = 50, zeroesPassed = 0}

isAtZero :: Dial -> Bool
isAtZero Dial {..} = position == 0

rotate :: Dial -> DialRotation -> Dial
rotate Dial {..} DialRotation {..} =
  let clicksBackToZero =
        case direction of
          DialLeft  -> (100 - position) `mod` 100
          DialRight -> position `mod` 100
      additionalPasses = (clicks + clicksBackToZero) `div` 100
      newPosition =
        case direction of
          DialLeft  -> (position - clicks) `mod` 100
          DialRight -> (position + clicks) `mod` 100
   in Dial
        {position = newPosition, zeroesPassed = zeroesPassed + additionalPasses}

doRotations :: [DialRotation] -> [Dial]
doRotations = doRotationsFrom initialDial
  where
    doRotationsFrom :: Dial -> [DialRotation] -> [Dial]
    doRotationsFrom current [] = [current]
    doRotationsFrom current (next:rest) =
      current : doRotationsFrom (rotate current next) rest
