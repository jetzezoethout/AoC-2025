module Machine where

import           Data.IntSet   (IntSet)
import qualified Data.IntSet   as S
import           Data.List     (elemIndices)
import           Data.Sequence (Seq, ViewL (..), viewl, (><))
import qualified Data.Sequence as Seq
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Lights        (Button, Lights, fromLightsOn, lightsOff, push)
import           Parsers       (parseInt)

data Machine = Machine
  { targetDiagram :: Lights
  , buttons       :: [Button]
  } deriving (Show)

parseTargetDiagram :: Text -> Lights
parseTargetDiagram text = fromLightsOn $ elemIndices '#' $ T.unpack properDiagram
  where
    properDiagram = T.drop 1 $ T.dropEnd 1 text

parseButton :: Text -> Button
parseButton text = fromLightsOn $ map parseInt (T.splitOn "," properButton)
  where
    properButton = T.drop 1 $ T.dropEnd 1 text

parseMachine :: Text -> Machine
parseMachine text =
  let parts = T.words text
   in Machine {targetDiagram = parseTargetDiagram $ head parts, buttons = map parseButton $ init $ tail parts}

data ButtonPresses = ButtonPresses
  { buttonsPressed :: Int
  , lights         :: Lights
  }

initialPresses :: ButtonPresses
initialPresses = ButtonPresses {buttonsPressed = 0, lights = lightsOff}

press :: ButtonPresses -> Button -> ButtonPresses
press ButtonPresses {..} button = ButtonPresses {buttonsPressed = buttonsPressed + 1, lights = lights `push` button}

configure :: Machine -> Int
configure Machine {..} = go S.empty $ Seq.singleton ButtonPresses {buttonsPressed = 0, lights = lightsOff}
  where
    go :: IntSet -> Seq ButtonPresses -> Int
    go seen queue =
      case viewl queue of
        EmptyL -> error "target diagram unreachable"
        (current :< remaining) ->
          if currentLights == targetDiagram
            then buttonsPressed current
            else go (seen `S.union` S.fromList (map (currentLights `push`) buttons))
                   $ remaining >< Seq.fromList (filter ((`S.notMember` seen) . lights) $ map (current `press`) buttons)
          where currentLights = lights current
