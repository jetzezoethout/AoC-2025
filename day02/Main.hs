module Main where

import qualified Data.Text   as T
import           IdRange     (isInvalid, parseIdRange, productIdsInRange)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let idRanges = map parseIdRange $ T.splitOn "," text
        productIds = idRanges >>= productIdsInRange
        invalidProductIds = filter isInvalid productIds
    print $ sum invalidProductIds
