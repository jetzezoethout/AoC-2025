module Main where

import           BatteryBank (maxJoltage, parseBatteryBank)
import qualified Data.Text   as T
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let batteryBanks = map parseBatteryBank $ T.lines text
    print $ sum $ map maxJoltage batteryBanks
