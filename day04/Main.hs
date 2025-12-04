module Main where

import qualified Data.Set     as S
import           PaperRollMap (cleanUp, freeRolls, parsePaperRollMap)
import           ProcessFile  (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let paperRollMap = parsePaperRollMap text
    print $ freeRolls paperRollMap
    print $ S.size paperRollMap - S.size (cleanUp paperRollMap)
