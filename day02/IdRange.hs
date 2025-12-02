module IdRange where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

type ProductId = Int

data IdRange = IdRange
  { start :: ProductId
  , end   :: ProductId
  } deriving (Show)

maxIdLength :: IdRange -> Int
maxIdLength IdRange {..} = length $ show end

parseIdRange :: Text -> IdRange
parseIdRange text =
  let parts = T.splitOn "-" text
   in IdRange {start = parseInt $ head parts, end = parseInt $ parts !! 1}

sumSillyPatterns :: IdRange -> Int -> Int -> Int
sumSillyPatterns IdRange {..} patternRepeats patternLength =
  if firstPattern > lastPattern
    then 0
    else ((firstPattern + lastPattern) * (lastPattern - firstPattern + 1) * factor) `div` 2
  where
    factor = sum $ map ((10 ^) . (patternLength *)) [0 .. patternRepeats - 1]
    firstPattern = max (10 ^ (patternLength - 1)) $ start `ceilDiv` factor
    lastPattern = min (10 ^ patternLength - 1) $ end `div` factor

ceilDiv :: Integral a => a -> a -> a
x `ceilDiv` y = -((-x) `div` y)

sumInvalid :: IdRange -> Int
sumInvalid idRange = sum $ map (sumSillyPatterns idRange 2) [1 .. maxIdLength idRange `div` 2]

sumExtendedInvalid :: IdRange -> Int
sumExtendedInvalid idRange =
  sum
    [ -(mu patternRepeats * sumSillyPatterns idRange patternRepeats patternLength)
    | patternRepeats <- [2 .. maxIdLength idRange]
    , patternLength <- [1 .. maxIdLength idRange `div` patternRepeats]
    ]

mu :: Int -> Int
mu 1 = 1
mu n =
  if largestDivisor `mod` smallestDivisor == 0
    then 0
    else negate $ mu largestDivisor
  where
    smallestDivisor = head $ filter ((== 0) . (n `mod`)) [2 ..]
    largestDivisor = n `div` smallestDivisor
