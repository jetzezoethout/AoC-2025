module Components where

import           Coordinate3D (Coordinate3D)
import           Data.List    (foldl')
import           Data.Map     (Map, (!))
import qualified Data.Map     as M
import           Edge         (Edge (..))

type ComponentId = Int

data Components = Components
  { coordinateToComponent :: Map Coordinate3D ComponentId
  , componentToSize       :: Map ComponentId Int
  }

disconnected :: [Coordinate3D] -> Components
disconnected coordinates =
  Components
    { coordinateToComponent = M.fromList $ zip coordinates [0 ..]
    , componentToSize = M.fromList $ map (, 1) [0 .. length coordinates - 1]
    }

componentSizes :: Components -> [Int]
componentSizes Components {..} = M.elems componentToSize

isConnected :: Components -> Bool
isConnected Components {..} = M.size componentToSize == 1

connectEdge :: Components -> Edge -> Components
connectEdge components@Components {..} Edge {..} =
  if sourceComponent == targetComponent
    then components
    else Components
           { coordinateToComponent = M.map updateComponent coordinateToComponent
           , componentToSize =
               M.delete targetComponent $ M.adjust (+ componentToSize ! targetComponent) sourceComponent componentToSize
           }
  where
    sourceComponent = coordinateToComponent ! source
    targetComponent = coordinateToComponent ! target
    updateComponent component =
      if component == targetComponent
        then sourceComponent
        else component

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
