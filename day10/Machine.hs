module Machine where

import           Data.List (elemIndices)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

newtype LightsDiagram = LightsOn
  { lightsOn :: [Int]
  } deriving (Show)

blackout :: LightsDiagram
blackout = LightsOn []

isOnAt :: LightsDiagram -> Int -> Bool
LightsOn {..} `isOnAt` i = i `elem` lightsOn

type Lights = LightsDiagram

type Button = LightsDiagram

type Joltages = [Int]

data Machine = Machine
  { targetDiagram      :: Lights
  , buttons            :: [Button]
  , joltageRequirement :: Joltages
  } deriving (Show)

parseTargetDiagram :: Text -> Lights
parseTargetDiagram text = LightsOn $ elemIndices '#' $ T.unpack properDiagram
  where
    properDiagram = T.drop 1 $ T.dropEnd 1 text

parseButton :: Text -> Button
parseButton text = LightsOn $ map parseInt (T.splitOn "," properButton)
  where
    properButton = T.drop 1 $ T.dropEnd 1 text

parseJoltageRequirement :: Text -> Joltages
parseJoltageRequirement text = map parseInt (T.splitOn "," properJoltages)
  where
    properJoltages = T.drop 1 $ T.dropEnd 1 text

parseMachine :: Text -> Machine
parseMachine text =
  let parts = T.words text
   in Machine
        { targetDiagram = parseTargetDiagram $ head parts
        , buttons = map parseButton $ init $ tail parts
        , joltageRequirement = parseJoltageRequirement $ last parts
        }
