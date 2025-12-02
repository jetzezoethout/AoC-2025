module Main where

import qualified Data.Text   as T
import           IdRange     (parseIdRange, sumExtendedInvalid, sumInvalid)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let idRanges = map parseIdRange $ T.splitOn "," text
    print $ sum $ map sumInvalid idRanges
    print $ sum $ map sumExtendedInvalid idRanges
