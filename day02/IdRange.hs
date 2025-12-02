module IdRange where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

type ProductId = Int

isSilly :: ProductId -> Bool
isSilly = (`isRepeating` 2)

isExtendedSilly :: ProductId -> Bool
isExtendedSilly productId = any (productId `isRepeating`) [2 .. idLength]
  where
    idLength = length $ show productId

isRepeating :: ProductId -> Int -> Bool
productId `isRepeating` times =
  concat (replicate times $ take (idLength `div` times) idString) == idString
  where
    idString = show productId
    idLength = length idString

data IdRange = IdRange
  { start :: ProductId
  , end   :: ProductId
  } deriving (Show)

parseIdRange :: Text -> IdRange
parseIdRange text =
  let parts = T.splitOn "-" text
   in IdRange {start = parseInt $ head parts, end = parseInt $ parts !! 1}

productIdsInRange :: IdRange -> [ProductId]
productIdsInRange IdRange {..} = [start .. end]
