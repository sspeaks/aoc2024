module Day23 where

import Control.Arrow ((&&&))
import Data.Array qualified as A
import Data.Function (on)
import Data.Graph qualified as G
import Data.IntSet qualified as S
import Data.List (intercalate, mapAccumL, maximumBy, nub, sort)
import Data.Map qualified as M
import Data.Vector qualified as V
import Text.Parsec (Parsec, alphaNum, char, many1, runParser)
import Text.Parsec.Char (spaces)

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

type Parser = Parsec String ()

type Computer = String

data Connection = Conn Computer Computer deriving (Show)

data Trio = Trio Computer Computer Computer deriving (Show)

instance Eq Trio where
  (==) :: Trio -> Trio -> Bool
  (Trio a b c) == (Trio d e f) = sort [a, b, c] == sort [d, e, f]

type Input = (G.Graph, G.Vertex -> (String, String, [String]), String -> Maybe G.Vertex)

solve :: Input -> [Trio]
solve (g, nfv, _) =
  let edges = A.assocs g
      applyOnce = edges
   in concatMap
        ( uncurry (canReachBack g nfv)
        )
        applyOnce

vertexToComputer :: (G.Vertex -> (String, String, [String])) -> G.Vertex -> Computer
vertexToComputer nfv i = (\(a, _, _) -> a) $ nfv i

canReachBack :: G.Graph -> (G.Vertex -> (String, String, [String])) -> G.Vertex -> [G.Vertex] -> [Trio]
canReachBack g nfv v vs =
  [ Trio (vertexToComputer nfv v) (vertexToComputer nfv v1) (vertexToComputer nfv v2)
    | v1 <- vs,
      v2 <- vs,
      v1 /= v2,
      connected g v v1,
      connected g v v2,
      connected g v1 v2
  ]

connected :: G.Graph -> G.Vertex -> G.Vertex -> Bool
connected g v1 v2 =
  let v1Neighs = g A.! v1
   in v2 `elem` v1Neighs

containsT :: Trio -> Bool
containsT (Trio a b c) = any test [a, b, c]
  where
    test [] = False
    test ('t' : _) = True
    test _ = False

reachable :: G.Graph -> G.Vertex -> [G.Vertex]
reachable g v = g A.! v

part1 :: Input -> Int
part1 inp = length . filter containsT . nub $ solve inp

type Clique = [G.Vertex]

part2 :: Input -> String
part2 (g, nfv, _) =
  let cliques = getMaximalCliques f nodes
   in intercalate "," . sort . map (vertexToComputer nfv) $ maximumBy (compare `on` length) cliques
  where
    nodes = A.indices g
    f a b = b `elem` (g A.! a)

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = do
      conns <- many1 (parseConn <* spaces)
      let vertices = M.assocs $ M.fromListWith (<>) $ conns >>= \(Conn a b) -> [(a, [b]), (b, [a])]
      return $ G.graphFromEdges $ map (\(a, b) -> (a, a, b)) vertices
    parseConn :: Parser Connection
    parseConn = Conn <$> (many1 alphaNum <* char '-') <*> many1 alphaNum

main :: IO ()
main = readFile "inputs/day23input.txt" >>= print . (part1 &&& part2) . prepare