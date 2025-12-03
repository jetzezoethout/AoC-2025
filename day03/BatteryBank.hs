module BatteryBank where

import           Data.Char (digitToInt)
import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text as T

type Battery = Int

type BatteryBank = [Battery]

parseBatteryBank :: Text -> BatteryBank
parseBatteryBank = map digitToInt . T.unpack

maxJoltage :: Int -> BatteryBank -> Int
maxJoltage batteriesNeeded batteryBank = foldl' (\x y -> 10 * x + y) 0 $ selectBatteries batteriesNeeded batteryBank

selectBatteries :: Int -> BatteryBank -> [Battery]
selectBatteries 0 _ = []
selectBatteries batteriesNeeded batteryBank =
  let (first, remaining) = selectFirstBattery batteriesNeeded batteryBank
   in first : selectBatteries (batteriesNeeded - 1) remaining

selectFirstBattery :: Int -> BatteryBank -> (Battery, BatteryBank)
selectFirstBattery _ [] = error "bank is empty"
selectFirstBattery batteriesNeeded (x:xs) = go x xs xs
  where
    go maxSoFar tailSoFar ys
      | length ys <= batteriesNeeded - 1 = (maxSoFar, tailSoFar)
      | otherwise =
        case ys of
          [] -> (maxSoFar, tailSoFar)
          (next:rest) ->
            if next > maxSoFar
              then go next rest rest
              else go maxSoFar tailSoFar rest
