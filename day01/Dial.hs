module Dial where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

type Dial = Int

data DialDirection
  = DialLeft
  | DialRight
  deriving (Show)

parseDialDirection :: Char -> DialDirection
parseDialDirection 'L' = DialLeft
parseDialDirection 'R' = DialRight
parseDialDirection _   = error "Unknown direction"

data DialRotation = DialRotation
  { direction :: DialDirection
  , clicks    :: Int
  } deriving (Show)

parseDialRotation :: Text -> DialRotation
parseDialRotation text =
  DialRotation
    { direction = parseDialDirection $ T.head text
    , clicks = parseInt $ T.tail text
    }

rotate :: Dial -> DialRotation -> Dial
rotate current DialRotation {..} =
  case direction of
    DialLeft  -> (current - clicks) `mod` 100
    DialRight -> (current + clicks) `mod` 100

doRotations :: [DialRotation] -> [Dial]
doRotations = doRotationsFrom 50
  where
    doRotationsFrom :: Dial -> [DialRotation] -> [Dial]
    doRotationsFrom current [] = [current]
    doRotationsFrom current (next:rest) =
      current : doRotationsFrom (rotate current next) rest
