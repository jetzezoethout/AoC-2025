module Lights where

import           Data.Bits (Bits (bit, xor, zeroBits, (.|.)))

-- | A lighting configuration is represented by its bitmask
type Lights = Int

fromLightsOn :: [Int] -> Lights
fromLightsOn = foldr ((.|.) . bit) zeroBits

lightsOff :: Lights
lightsOff = zeroBits

type Button = Lights

push :: Lights -> Button -> Lights
push = xor
