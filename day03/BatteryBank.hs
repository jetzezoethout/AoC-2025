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
  let indexedFirst = selectFirstBattery batteriesNeeded batteryBank
   in value indexedFirst : selectBatteries (batteriesNeeded - 1) (drop (index indexedFirst + 1) batteryBank)

selectFirstBattery :: Int -> BatteryBank -> Indexed Battery
selectFirstBattery batteriesNeeded xs =
  case enumerate xs of
    []         -> error "bank is empty"
    first:rest -> go first first rest
  where
    totalLength = length xs
    go maxSoFar current remaining =
      if index current + batteriesNeeded >= totalLength
        then maxSoFar
        else let next = head remaining
                 stillRemaining = tail remaining
              in if value next > value maxSoFar
                   then go next next stillRemaining
                   else go maxSoFar next stillRemaining

data Indexed a = Indexed
  { index :: Int
  , value :: a
  }

enumerate :: [a] -> [Indexed a]
enumerate = zipWith Indexed [0 ..]
