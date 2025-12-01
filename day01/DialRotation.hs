module DialRotation where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

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
