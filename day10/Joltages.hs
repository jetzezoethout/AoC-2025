module Joltages where

import           Data.Maybe        (fromJust)
import           Data.Ratio        (denominator, numerator)
import           LinAlg.Equation   (Equation (..), System, system)
import           LinAlg.Expression (evaluate)
import           LinAlg.RowVector  (RowVector (..), unit)
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

isValid :: Solution -> RowVector -> Bool
isValid expressions values = all (isButtonPresses . (`evaluate` values)) expressions

combinations :: Int -> Int -> [[Int]]
combinations 0 0 = [[]]
combinations 0 _ = []
combinations vars total = [x : xs | x <- [0 .. total], xs <- combinations (vars - 1) (total - x)]

data IntTop
  = Finite Int
  | Infinity
  deriving (Eq, Ord)

safeMinimum :: [IntTop] -> IntTop
safeMinimum = foldr min Infinity

asInt :: IntTop -> Int
asInt (Finite n) = n
asInt Infinity   = error "infinity is not a number"

configureJoltages :: Machine -> Int
configureJoltages machine = go Infinity 0
  where
    solution = fromJust $ solve $ makeSystem machine
    freedoms = nullDimension solution
    toInt (Scalar l) = fromIntegral $ numerator l `div` denominator l
    go :: IntTop -> Int -> Int
    go minSoFar currentTotal
      | minSoFar <= Finite currentTotal = asInt minSoFar
      | otherwise =
        let minForCurrentTotal =
              safeMinimum
                $ map (Finite . toInt . evaluate (sumVecs solution))
                $ filter (isValid solution)
                $ map (RowVector . map toScalar)
                $ combinations freedoms currentTotal
         in go (min minSoFar minForCurrentTotal) (currentTotal + 1)
