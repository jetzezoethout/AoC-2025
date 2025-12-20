module Indicator where

import           Data.Bits     (Bits (bit, xor, zeroBits, (.|.)))
import           Data.IntSet   (IntSet)
import qualified Data.IntSet   as S
import           Data.Sequence (Seq, ViewL (..), viewl, (><))
import qualified Data.Sequence as Seq
import           Machine       (LightsDiagram (lightsOn), Machine (..),
                                blackout)

type LightsBitmask = Int

fromLightsOn :: LightsDiagram -> LightsBitmask
fromLightsOn = foldr ((.|.) . bit) zeroBits . lightsOn

type ButtonBitmask = Int

push :: LightsBitmask -> ButtonBitmask -> LightsBitmask
push = xor

data ButtonPresses = ButtonPresses
  { buttonsPressed :: Int
  , lights         :: LightsBitmask
  }

initialPresses :: ButtonPresses
initialPresses = ButtonPresses {buttonsPressed = 0, lights = fromLightsOn blackout}

press :: ButtonPresses -> ButtonBitmask -> ButtonPresses
press ButtonPresses {..} button = ButtonPresses {buttonsPressed = buttonsPressed + 1, lights = lights `push` button}

configureIndicators :: Machine -> Int
configureIndicators Machine {..} = go S.empty $ Seq.singleton initialPresses
  where
    go :: IntSet -> Seq ButtonPresses -> Int
    go seen queue =
      case viewl queue of
        EmptyL -> error "target diagram unreachable"
        (current :< remaining) ->
          if currentLights == fromLightsOn targetDiagram
            then buttonsPressed current
            else go (seen `S.union` S.fromList (map ((currentLights `push`) . fromLightsOn) buttons))
                   $ remaining
                       >< Seq.fromList
                            (filter ((`S.notMember` seen) . lights) $ map ((current `press`) . fromLightsOn) buttons)
          where currentLights = lights current
