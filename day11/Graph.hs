module Graph where

import           Cached           (Cached, runMemoized, withCache)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Traversable (for)

type Node = Text

type Graph = Map Node [Node]

neighbours :: Graph -> Node -> [Node]
neighbours = flip $ M.findWithDefault []

parseGraph :: Text -> Graph
parseGraph text = M.fromList $ map parseLine $ T.lines text
  where
    parseLine textLine =
      let parts = T.splitOn ": " textLine
       in (head parts, T.words $ parts !! 1)

numberOfPaths :: Graph -> Node -> Node -> Int
numberOfPaths graph start end = runMemoized $ go start
  where
    go :: Node -> Cached Node Int
    go node =
      if node == end
        then return 1
        else do
          paths <- for (neighbours graph node) $ \neighbour -> withCache go neighbour
          return $ sum paths

numberOfProblematicPaths :: Graph -> Int
numberOfProblematicPaths graph =
  let fftToDac = numberOfPaths graph "fft" "dac"
   in if fftToDac > 0
        then numberOfPaths graph "svr" "fft" * fftToDac * numberOfPaths graph "dac" "out"
        else numberOfPaths graph "svr" "dac" * numberOfPaths graph "dac" "fft" * numberOfPaths graph "fft" "out"
