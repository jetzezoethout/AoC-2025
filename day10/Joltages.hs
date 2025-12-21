module Joltages where

import           Data.List         (foldl')
import           Data.Maybe        (fromJust)
import           Data.Ratio        (denominator, numerator)
import           Data.Traversable  (for)
import           LinAlg.Constraint (collectLowers, collectUppers, constrain)
import           LinAlg.Equation   (Equation (..), System, system)
import           LinAlg.Expression (Expression (..), commonDenominator,
                                    evaluate)
import           LinAlg.RowVector  (RowVector (..), embedAt, leading, unit)
import           LinAlg.Solve      (Solution, nullDimension, solve)
import           LinAlg.Vector     (Scalar (..), sumVecs, toScalar, (^*))
import           Machine           (Machine (..), isOnAt)

makeSystem :: Machine -> System
makeSystem Machine {..} = system numVars $ map makeEq [0 .. length joltageRequirement - 1]
  where
    numVars = length buttons
    makeEq i =
      let coefficients =
            [ if (buttons !! j) `isOnAt` i
              then 1
              else 0
            | j <- [0 .. numVars - 1]
            ]
       in Equation
            { eqCoefficients = sumVecs $ zipWith (^*) coefficients [unit numVars j | j <- [0 .. numVars - 1]]
            , rhs = toScalar $ joltageRequirement !! i
            }

isButtonPresses :: Scalar -> Bool
isButtonPresses (Scalar l) = l >= 0 && numerator l `mod` denominator l == 0

isValidFor :: Solution -> RowVector -> Bool
isValidFor solution values = all (isButtonPresses . (`evaluate` values)) solution

roundDown :: Scalar -> Scalar
roundDown (Scalar l) = toScalar $ numerator l `div` denominator l

roundUp :: Scalar -> Scalar
roundUp (Scalar l) = toScalar $ numerator l `ceilDiv` denominator l
  where
    x `ceilDiv` y = -((-x) `div` y)

toInt :: Scalar -> Int
toInt (Scalar l) =
  if numerator l `mod` denominator l == 0
    then fromInteger $ numerator l `div` denominator l
    else error "Non-integral scalar"

configureJoltages :: Machine -> Int
configureJoltages machine =
  toInt
    $ if freedoms == 0
        then constant sumExpression
        else minimum $ map (evaluate sumExpression) $ filter (isValidFor solution) criticalPoints
  where
    solution = fromJust $ solve $ makeSystem machine
    freedoms = nullDimension solution
    maxJoltage = maximum $ joltageRequirement machine
    sumExpression = sumVecs solution
    constraints = map constrain solution
    lowerBounds = collectLowers constraints
    upperBounds = collectUppers constraints
    criticalWidth = foldl' lcm 1 $ map commonDenominator solution
    getCriticalValues fixedParams =
      if leading (paramCoefficients sumExpression) >= 0
        then let edgeValue = roundUp $ maximum $ fmap (`evaluate` fixedParams) lowerBounds
              in map ((edgeValue +) . toScalar) [0 .. criticalWidth - 1]
        else let edgeValue = roundDown $ minimum $ fmap (`evaluate` fixedParams) upperBounds
              in map ((edgeValue -) . toScalar) [0 .. criticalWidth - 1]
    criticalPoints = do
      fixedParams <- RowVector <$> for [0 .. freedoms - 2] (const $ map toScalar [0 .. maxJoltage])
      criticalValue <- getCriticalValues fixedParams
      return $ embedAt criticalValue fixedParams
