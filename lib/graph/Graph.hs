{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Graph where
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

class Point p d | p -> d where
    dist :: p -> p -> d

data Graph a d where
    Graph :: Point a d => M.Map a (S.Set a) -> Graph a d

instance Floating a => Point (a,a,a) a where
  dist (x1,y1,z1) (x2,y2,z2) = sqrt $ (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2

createGraphWithAllEdges :: (Eq a, Ord a, Point a a) => [a] -> Graph a a
createGraphWithAllEdges = Graph . go . allEdges
    where allEdges ls = [(a, S.singleton b) | a <- ls, b <- ls, a /= b]
          go = M.fromListWith (<>)

{-
    Int: Number of connections to make
    [a]: List of pts to make connections from
    ([Graph a a], [a]): A forest of graphs created from the n connections (because it won't necessarily connect all pts), and the unconnected pts
-}
makeNConnectionsTowardRelativeConnection :: (Point a a) => Int -> [a] -> ([Graph a a], [a])
makeNConnectionsTowardRelativeConnection numConns pts = undefined