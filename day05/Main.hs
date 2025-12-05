module Main where

import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           IngredientRange (isFreshFor, parseIngredientRange,
                                  totalIngredients)
import           Parsers         (parseInt)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let parts = splitOn [""] $ T.lines text
        ingredientRanges = map parseIngredientRange $ head parts
        ingredientIds = map parseInt $ parts !! 1
    print $ length $ filter (`isFreshFor` ingredientRanges) ingredientIds
    print $ totalIngredients ingredientRanges
