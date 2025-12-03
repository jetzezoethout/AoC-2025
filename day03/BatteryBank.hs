module BatteryBank where

import           Data.Char (digitToInt)
import           Data.Text (Text)
import qualified Data.Text as T

type BatteryBank = [Int]

parseBatteryBank :: Text -> BatteryBank
parseBatteryBank = map digitToInt . T.unpack

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x, ) xs <> pairs xs

maxJoltage :: BatteryBank -> Int
maxJoltage batteryBank = maximum [10 * x + y | (x, y) <- pairs batteryBank]
