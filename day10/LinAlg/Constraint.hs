module LinAlg.Constraint where

import           Data.Maybe        (mapMaybe)
import           LinAlg.Expression (Expression (..))
import           LinAlg.RowVector  (leading, project)
import           LinAlg.Vector     ((^*))

data Constraint a
  = UpperBound a
  | LowerBound a
  | Unconstrained
  deriving (Functor, Show)

-- | Constrain the first parameter in an `Expression` in terms of the other parameters,
--   given that the expression must be >= 0.
constrain :: Expression -> Constraint Expression
constrain Expression {..}
  | leadingCoefficient > 0 =
    LowerBound
      $ (-recip leadingCoefficient) ^* Expression {paramCoefficients = project paramCoefficients, constant = constant}
  | leadingCoefficient < 0 =
    UpperBound
      $ (-recip leadingCoefficient) ^* Expression {paramCoefficients = project paramCoefficients, constant = constant}
  | otherwise = Unconstrained
  where
    leadingCoefficient = leading paramCoefficients

collectLowers :: [Constraint a] -> [a]
collectLowers = mapMaybe getLower
  where
    getLower (LowerBound bound) = Just bound
    getLower _                  = Nothing

collectUppers :: [Constraint a] -> [a]
collectUppers = mapMaybe getUpper
  where
    getUpper (UpperBound bound) = Just bound
    getUpper _                  = Nothing
