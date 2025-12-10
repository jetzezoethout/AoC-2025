module Machine2 where

import           Control.Monad (forM, forM_)
import           Data.Maybe    (fromJust)
import           Data.SBV      (ConstraintSet, constrain, getModelValue,
                                literal, minimize, optLexicographic, sInteger,
                                (.==), (.>=))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseInt)

type Button = [Int]

type Joltages = [Int]

data Machine2 = Machine2
  { buttons            :: [Button]
  , joltageRequirement :: Joltages
  } deriving (Show)

parseButton :: Text -> Button
parseButton text = map parseInt (T.splitOn "," properButton)
  where
    properButton = T.drop 1 $ T.dropEnd 1 text

parseJoltageRequirement :: Text -> Button
parseJoltageRequirement text = map parseInt (T.splitOn "," properJoltages)
  where
    properJoltages = T.drop 1 $ T.dropEnd 1 text

parseMachine2 :: Text -> Machine2
parseMachine2 text =
  let parts = tail $ T.words text
   in Machine2 {buttons = map parseButton $ init parts, joltageRequirement = parseJoltageRequirement $ last parts}

makeSystem :: Machine2 -> ConstraintSet
makeSystem Machine2 {..} = do
  vars <- forM [0 .. dimension - 1] $ \i -> sInteger $ "x" <> show i
  forM_ vars $ \var -> constrain $ var .>= 0
  forM_ [0 .. equations - 1] $ \j ->
    constrain
      $ sum (map snd $ filter (\(i, _) -> j `elem` buttons !! i) $ zip [0 ..] vars)
          .== literal (fromIntegral $ joltageRequirement !! j)
  minimize "goal" $ sum vars
  where
    dimension = length buttons
    equations = length joltageRequirement

configure2 :: Machine2 -> IO Integer
configure2 mach = do
  result <- optLexicographic $ makeSystem mach
  return $ fromJust $ getModelValue "goal" result
