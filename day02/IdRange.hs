module IdRange where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

type ProductId = Int

isInvalid :: ProductId -> Bool
isInvalid productId =
  even idLength && take halfLength idString == drop halfLength idString
  where
    idString = show productId
    idLength = length idString
    halfLength = idLength `div` 2

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
