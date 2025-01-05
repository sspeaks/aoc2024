module Clique where

import Data.Array qualified as A
import Data.Bifunctor (second)
import Data.Function (on)
import Data.IntSet qualified as S
import Data.List (mapAccumL, sortBy)
import Data.Maybe (fromMaybe)
import Data.Set qualified as GS
import Data.Vector qualified as V

-- | Given a list of nodes, and a function that determines whether there is an edge between any two nodes, yields a list of maximal cliques -- sets of nodes such that every node is connected to every other, and such that no other node may be added while maintaining this property.
getMaximalCliques :: (a -> a -> Bool) -> [a] -> [[a]]
getMaximalCliques tolFun xs =
  map (map (fst . (V.!) lv) . S.toList) $
    maximalCliques pickpivot (snd . (V.!) lv) (S.fromList $ map fst lnodes)
  where
    lnodes = zip [0 ..] xs
    lnodes' = map (\(k, n) -> (n, S.fromList $ filter (/= k) $ map fst $ filter (tolFun n . snd) lnodes)) lnodes
    lv = V.fromList lnodes'
    pickpivot p x = head $ S.elems p ++ S.elems x

-- | The Bron-Kerbosch algorithm for finding all maximal cliques in an undirected graph.
-- <http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm>. Works on nodes represented as 'Int's.
maximalCliques ::
  -- | A function that given two 'IntSet's, chooses a member of one as a pivot.
  (S.IntSet -> S.IntSet -> Int) ->
  -- | A function that given a node id, yields the set of its neighbors.
  (Int -> S.IntSet) ->
  -- | The set of all nodes in the graph.
  S.IntSet ->
  -- | An enumeration of all maximal cliques in the graph.
  [S.IntSet]
maximalCliques pickpivot neighborsOf nodeset = go S.empty nodeset S.empty
  where
    go r p x
      | S.null p && S.null x = [r]
      | otherwise =
          let pivot = pickpivot p x
              step' (p', x') v =
                let nv = neighborsOf v
                 in ((S.delete v p', S.insert v x'), go (S.insert v r) (S.intersection nv p') (S.intersection nv x'))
           in concat . snd $ mapAccumL step' (p, x) $ S.elems (p S.\\ neighborsOf pivot)


-- From paper "ARBORICITY AND SUBGRAPH LISTING ALGORITHMS*"
-- https://www.cs.cornell.edu/courses/cs6241/2019sp/readings/Chiba-1985-arboricity.pdf
getTriangles :: forall a. (Eq a, Ord a, A.Ix a) => A.Array a [a] -> [[a]]
getTriangles a1 = getTriangles' a1 sortedVertexes
  where
    getTriangles' :: (Eq a, Ord a, A.Ix a) => A.Array a [a] -> [a] -> [[a]]
    getTriangles' _ [] = []
    getTriangles' graph (v : vs)
      | null graph = []
      | otherwise =
          let neighbors = allNeighbors graph v
              secondNeighbors = (\(n1, n2, n3) -> n3) $ foldl foldFunc (GS.fromList neighbors, allNeighbors graph, []) neighbors
           in ([(v :)] <*> secondNeighbors) ++ getTriangles' (deleteVertexFromGraph graph v) vs
    deleteVertexFromGraph :: A.Array a [a] -> a -> A.Array a [a]
    deleteVertexFromGraph g v = fmap (filter (/= v)) g
    allNeighbors gr n = gr A.! n
    -- edges vertexes according to their degree (i.e. number of adjacent edges)
    sortedVertexes = sortBy (compare `on` (length . allNeighbors a1)) (map fst (A.assocs a1))
    -- Folds down the list of v's to find w's that are adjacent to u. if it us, we use the output of this to contruct (v,u,w)
    foldFunc :: (GS.Set a, a -> [a], [[a]]) -> a -> (GS.Set a, a -> [a], [[a]])
    foldFunc (marked, neighs, res) u = case filter (`GS.member` marked) $ neighs u of
      [] -> (GS.delete u marked, neighs, res)
      mems -> (GS.delete u marked, neighs, ((\w -> [u, w]) <$> mems) ++ res) -- returns a list of [u,w]s