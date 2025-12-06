module Main where

import           Problem     (evaluate, parseCorrectProblems,
                              parseWrongProblems)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let wrongProblems = parseWrongProblems text
        correctProblems = parseCorrectProblems text
    print $ sum $ map evaluate wrongProblems
    print $ sum $ map evaluate correctProblems
