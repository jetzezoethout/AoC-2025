module Components where

import           Coordinate3D (Coordinate3D)
import           Data.List    (foldl')
import           Data.Map     (Map, (!))
import qualified Data.Map     as M
import           Edge         (Edge (..))

-- | A node is labeled with...
data NodeLabel
  -- | ... a pointer to another node in the same component, if it's not the root node, or...
  = Pointer Coordinate3D
  -- | ... the size of the component, if it is the root.
  | Size Int

newtype Components = Components
  { labelling :: Map Coordinate3D NodeLabel
  }

discrete :: [Coordinate3D] -> Components
discrete coords = Components $ M.fromList $ map (, Size 1) coords

componentSizes :: Components -> [Int]
componentSizes Components {..} = M.elems $ M.mapMaybe getSize labelling
  where
    getSize (Pointer _) = Nothing
    getSize (Size size) = Just size

isConnected :: Components -> Bool
isConnected components@Components {..} = componentSize == M.size labelling
  where
    (node, _) = M.findMin labelling
    (_, componentSize) = getRootAndSize components node

getRootAndSize :: Components -> Coordinate3D -> (Coordinate3D, Int)
getRootAndSize Components {..} = go
  where
    go node =
      case labelling ! node of
        Pointer next -> go next
        Size size    -> (node, size)

connectEdge :: Components -> Edge -> Components
connectEdge components@Components {..} Edge {..}
  | sourceRoot == targetRoot = components
  | sourceComponentSize < targetComponentSize =
    Components $ M.insert sourceRoot (Pointer targetRoot) $ M.insert targetRoot (Size mergedSize) labelling
  | otherwise = Components $ M.insert targetRoot (Pointer sourceRoot) $ M.insert sourceRoot (Size mergedSize) labelling
  where
    (sourceRoot, sourceComponentSize) = getRootAndSize components source
    (targetRoot, targetComponentSize) = getRootAndSize components target
    mergedSize = sourceComponentSize + targetComponentSize

connectEdges :: Components -> [Edge] -> Components
connectEdges = foldl' connectEdge

findFinalEdge :: Components -> [Edge] -> Edge
findFinalEdge = go
  where
    go _ [] = error "graph is not connected"
    go components (edge:edges) =
      let newComponents = connectEdge components edge
       in if isConnected newComponents
            then edge
            else go newComponents edges
