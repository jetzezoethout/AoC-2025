module LinAlg.Solve where

import           Data.List         (foldl')
import           LinAlg.Equation   (Equation (..), System (..), dropFirst,
                                    system)
import           LinAlg.Expression (Expression (..), constExpression,
                                    paramExpression, shiftExpression)
import           LinAlg.RowVector  (RowVector (components), dimension, leading,
                                    project)
import           LinAlg.Vector     ((^*), (^-))

type Solution = [Expression]

nullDimension :: Solution -> Int
nullDimension sol =
  if null sol
    then 0
    else dimension $ paramCoefficients $ head sol

solve :: System -> Maybe Solution
solve System {..}
  | numVars == 0 =
    if any ((/= 0) . rhs) eqs
      then Nothing
      else Just []
  | otherwise =
    case pick ((/= 0) . leading . eqCoefficients) eqs of
      Nothing -> do
        subSolution <- solve $ system (numVars - 1) $ map dropFirst eqs
        let subFreedoms = nullDimension subSolution
            newExpression = paramExpression (subFreedoms + 1) 0
        return $ newExpression : map shiftExpression subSolution
      Just (pivot, remainingEqs) -> do
        let normalizedPivot = recip (leading (eqCoefficients pivot)) ^* pivot
            sweep eq = eq ^- (leading (eqCoefficients eq) ^* normalizedPivot)
        subSolution <- solve $ system (numVars - 1) $ map (dropFirst . sweep) remainingEqs
        let subFreedoms = nullDimension subSolution
            newExpression =
              foldl' (^-) (constExpression subFreedoms $ rhs normalizedPivot)
                $ zipWith (^*) (components $ project $ eqCoefficients normalizedPivot) subSolution
        return $ newExpression : subSolution

pick :: (a -> Bool) -> [a] -> Maybe (a, [a])
pick _ [] = Nothing
pick p (x:xs) =
  if p x
    then Just (x, xs)
    else do
      (needle, rest) <- pick p xs
      return (needle, x : rest)
