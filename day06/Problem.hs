module Problem where

import           Data.Char       (isSpace)
import           Data.List       (transpose)
import           Data.List.Split (splitWhen)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers         (parseInt)

data Operation
  = Add
  | Multiply
  deriving (Show)

parseOperation :: Text -> Operation
parseOperation "+" = Add
parseOperation "*" = Multiply
parseOperation _   = error "Unknown operation"

data Problem = Problem
  { inputs    :: [Int]
  , operation :: Operation
  } deriving (Show)

evaluate :: Problem -> Int
evaluate Problem {..} =
  case operation of
    Add      -> sum inputs
    Multiply -> product inputs

parseWrongProblems :: Text -> [Problem]
parseWrongProblems text = map parseProblem $ transpose $ map T.words $ T.lines text
  where
    parseProblem :: [Text] -> Problem
    parseProblem column = Problem {inputs = map parseInt $ init column, operation = parseOperation $ last column}

parseCorrectProblems :: Text -> [Problem]
parseCorrectProblems text = zipWith Problem inputsList operationList
  where
    textLines = T.lines text
    inputsList = map parseInputs $ splitWhen (all isSpace) $ transpose $ map T.unpack $ init textLines
    operationList = map parseOperation $ T.words $ last textLines
    parseInputs :: [String] -> [Int]
    parseInputs = map parseInput
    parseInput :: String -> Int
    parseInput column = parseInt $ T.dropAround isSpace $ T.pack column
