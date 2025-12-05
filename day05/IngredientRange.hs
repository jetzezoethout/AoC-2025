module IngredientRange where

import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

type IngredientId = Int

data IngredientRange = IngredientRange
  { start :: IngredientId
  , end   :: IngredientId
  } deriving (Show, Eq, Ord)

parseIngredientRange :: Text -> IngredientRange
parseIngredientRange text =
  let parts = T.splitOn "-" text
   in IngredientRange {start = parseInt $ head parts, end = parseInt $ parts !! 1}

numberOfIngredients :: IngredientRange -> Int
numberOfIngredients IngredientRange {..} = end - start + 1

isInside :: IngredientId -> IngredientRange -> Bool
ingredientId `isInside` IngredientRange {..} = start <= ingredientId && ingredientId <= end

isFreshFor :: IngredientId -> [IngredientRange] -> Bool
ingredientId `isFreshFor` ingredientRanges = any (ingredientId `isInside`) ingredientRanges

-- | Transform a list of ingredient ranges into a sorted list of ranges, none of which overlap.
normalize :: [IngredientRange] -> [IngredientRange]
normalize ranges = go $ sort ranges
  where
    go [] = []
    go [range] = [range]
    go (range1:range2:rest) =
      if end range1 >= start range2
        then go $ IngredientRange {start = start range1, end = max (end range1) (end range2)} : rest
        else range1 : go (range2 : rest)

totalIngredients :: [IngredientRange] -> Int
totalIngredients ranges = sum $ map numberOfIngredients $ normalize ranges
