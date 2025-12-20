module Joltages where

import           Control.Monad (forM, forM_)
import           Data.Maybe    (fromJust)
import           Data.SBV      (ConstraintSet, constrain, getModelValue,
                                literal, minimize, optLexicographic, sInteger,
                                (.==), (.>=))
import           Machine       (Machine (..), isOnAt)

makeSystem :: Machine -> ConstraintSet
makeSystem Machine {..} = do
  vars <- forM [0 .. dimension - 1] $ \i -> sInteger $ "x" <> show i
  forM_ vars $ \var -> constrain $ var .>= 0
  forM_ [0 .. equations - 1] $ \j ->
    constrain
      $ sum (map snd $ filter (\(i, _) -> (buttons !! i) `isOnAt` j) $ zip [0 ..] vars)
          .== literal (fromIntegral $ joltageRequirement !! j)
  minimize "goal" $ sum vars
  where
    dimension = length buttons
    equations = length joltageRequirement

configure2 :: Machine -> IO Integer
configure2 mach = do
  result <- optLexicographic $ makeSystem mach
  return $ fromJust $ getModelValue "goal" result
