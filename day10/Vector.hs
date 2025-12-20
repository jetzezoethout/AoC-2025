module Vector where

import           Control.Monad    (guard)
import           Data.Foldable    (Foldable (foldl'))
import           Data.Ratio       (denominator, numerator, (%))
import           Data.Traversable (for)
import           Machine          (Machine (..), isOnAt)

newtype Scalar =
  Scalar Rational
  deriving (Eq, Show, Num, Fractional)

class Scalarable a where
  toScalar :: a -> Scalar

instance Scalarable Scalar where
  toScalar :: Scalar -> Scalar
  toScalar = id

instance Scalarable Int where
  toScalar :: Int -> Scalar
  toScalar x = Scalar $ fromIntegral x % 1

infixl 6 ^+
infixl 7 ^*
class Eq v =>
      Vector v
  where
  (^+) :: v -> v -> v
  (^*) :: Scalar -> v -> v
  (^-) :: v -> v -> v
  x ^- y = x ^+ ((-1) ^* y)

instance Vector Scalar where
  (^+) :: Scalar -> Scalar -> Scalar
  Scalar l ^+ Scalar m = Scalar $ l + m
  (^*) :: Scalar -> Scalar -> Scalar
  Scalar l ^* Scalar m = Scalar $ l * m

newtype RowVector = RowVector
  { components :: [Scalar]
  } deriving (Eq, Show)

instance Vector RowVector where
  (^+) :: RowVector -> RowVector -> RowVector
  RowVector v ^+ RowVector w = RowVector $ zipWith (+) v w
  (^*) :: Scalar -> RowVector -> RowVector
  l ^* RowVector v = RowVector $ map (l *) v

zero :: Int -> RowVector
zero dim = RowVector $ replicate dim 0

unit :: Int -> Int -> RowVector
unit dim i =
  RowVector
    [ if i == j
      then 1
      else 0
    | j <- [0 .. dim - 1]
    ]

dimension :: RowVector -> Int
dimension (RowVector v) = length v

leading :: RowVector -> Scalar
leading (RowVector v) = head v

following :: RowVector -> RowVector
following (RowVector v) = RowVector $ tail v

shiftVector :: RowVector -> RowVector
shiftVector (RowVector v) = RowVector $ 0 : v

dotProduct :: RowVector -> RowVector -> Scalar
dotProduct (RowVector v) (RowVector w) = sum $ zipWith (*) v w

data Affine = Affine
  { linear   :: RowVector
  , constant :: Scalar
  } deriving (Eq, Show)

instance Vector Affine where
  (^+) :: Affine -> Affine -> Affine
  Affine v l ^+ Affine w m = Affine (v ^+ w) (l ^+ m)
  (^*) :: Scalar -> Affine -> Affine
  l ^* Affine v m = Affine (l ^* v) (l ^* m)

-- | An equation can be represented by a rowvector and a right-hand side
type Equation = Affine

dropFirst :: Equation -> Equation
dropFirst (Affine v l) = Affine (following v) l

givesSum :: Equation -> Bool
givesSum eq = all (== 1) $ components $ linear eq

-- | An expression depending on variables can be represented by the coefficients and a constant part
type Expression = Affine

shiftExpression :: Expression -> Expression
shiftExpression (Affine v l) = Affine (shiftVector v) l

consExpr :: Int -> Scalar -> Expression
consExpr dim = Affine (zero dim)

evaluate :: RowVector -> Expression -> Scalar
evaluate v (Affine w l) = dotProduct v w + l

sumAll :: Solution -> Expression
sumAll = foldr1 (^+)

type System = [Equation]

type Solution = [Expression]

solve :: System -> Solution
solve system = go (dimension $ linear $ head system) system
  where
    go :: Int -> System -> Solution
    go vars eqs
      | vars == 0 =
        if any ((/= 0) . constant) eqs
          then error "unsolvable"
          else []
      | otherwise =
        case pick ((/= 0) . leading . linear) eqs of
          Nothing ->
            let subSolution = go (vars - 1) $ map dropFirst eqs
                subFreedoms =
                  if null subSolution
                    then 0
                    else dimension (linear $ head subSolution)
             in Affine (unit (subFreedoms + 1) 0) 0 : map shiftExpression subSolution
          Just (pivot, others) ->
            let normalizedPivot = (1 / leading (linear pivot)) ^* pivot
                reduce eq = eq ^- (leading (linear eq) ^* normalizedPivot)
                subSolution = go (vars - 1) $ map (dropFirst . reduce) others
                subFreedoms =
                  if null subSolution
                    then 0
                    else dimension $ linear $ head subSolution
             in foldl'
                  (^-)
                  (consExpr subFreedoms (constant normalizedPivot))
                  (zipWith (^*) (components $ following $ linear normalizedPivot) subSolution)
                  : subSolution

pick :: (a -> Bool) -> [a] -> Maybe (a, [a])
pick _ [] = Nothing
pick p (x:xs) =
  if p x
    then Just (x, xs)
    else do
      (needle, rest) <- pick p xs
      return (needle, x : rest)

makeSystem :: Machine -> System
makeSystem Machine {..} = map makeEq [0 .. length joltageRequirement - 1]
  where
    dim = length buttons
    makeEq i =
      let coefficients :: [Int] =
            [ if (buttons !! j) `isOnAt` i
              then 1
              else 0
            | j <- [0 .. dim - 1]
            ]
       in Affine
            (foldr (^+) (zero dim) $ zipWith (^*) (map toScalar coefficients) [unit dim j | j <- [0 .. dim - 1]])
            (toScalar $ joltageRequirement !! i)

isButtonPresses :: Scalar -> Bool
isButtonPresses (Scalar l) = l >= 0 && numerator l `mod` denominator l == 0

isValid :: Solution -> RowVector -> Bool
isValid expressions values = all (isButtonPresses . evaluate values) expressions

configureJoltages :: Machine -> Integer
configureJoltages machine = minimum $ map (toInt . (`evaluate` sumAll solution)) validValues
  where
    solution = solve system
    maxValue = maximum $ joltageRequirement machine
    freedoms = dimension $ linear $ head solution
    validValues = do
      values <- RowVector <$> for [0 .. freedoms - 1] (const $ map toScalar [0 .. maxValue])
      guard $ isValid solution values
      return values
    system = makeSystem machine
    toInt (Scalar l) = numerator l `div` denominator l

complexity :: Machine -> Int
complexity machine = dimension $ linear $ head $ solve $ makeSystem machine
