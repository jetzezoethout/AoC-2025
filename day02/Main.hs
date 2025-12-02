module Main where

import qualified Data.Text   as T
import           IdRange     (isExtendedSilly, isSilly, parseIdRange,
                              productIdsInRange)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let idRanges = map parseIdRange $ T.splitOn "," text
        productIds = idRanges >>= productIdsInRange
        sillyProductIds = filter isSilly productIds
        extendedSillyProductIds = filter isExtendedSilly productIds
    print $ sum sillyProductIds
    print $ sum extendedSillyProductIds
