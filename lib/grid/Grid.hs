{-# LANGUAGE LambdaCase #-}

module Grid where

import Data.Array.IArray qualified as A
import Data.Array.ST (STArray, modifyArray', readArray, runSTArray, thaw, writeArray)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (foldl', sortBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as Set

data Direction = N | S | E | W deriving (Show, Eq)

type Grid a = A.Array (Int, Int) a

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+. (c, d) = (a + c, b + d)

-- Yields an Array where the index is (x i.e. col, y i.e. row)
-- starting from top let and moving down and right
stringToGrid :: (Char -> a) -> String -> Grid a
stringToGrid convert s = A.array bounds ray
  where
    rows = length $ lines s
    cols = length . head $ lines s
    bounds = ((0, 0), (cols - 1, rows - 1))
    ray =
      [ ((x, y), v)
        | (y, ln) <- zip [0 ..] (lines s),
          (x, v) <- zip [0 ..] (convert <$> ln)
      ]

findInGrid :: forall a. (a -> Bool) -> Grid a -> [(Int, Int)]
findInGrid f g = foldl go [] (A.assocs g)
  where
    go :: [(Int, Int)] -> ((Int, Int), a) -> [(Int, Int)]
    go agg v@(c, x)
      | f x = c : agg
      | otherwise = agg

moveCell :: Grid a -> (Int, Int) -> Direction -> (a -> a) -> Maybe (Grid a)
moveCell g c dir toEmpty
  | not $ inBounds g newC = Nothing
  | otherwise = Just $ runSTArray $ do
      ray <- thaw g
      v <- readArray ray c
      modifyArray' ray c toEmpty
      writeArray ray newC v
      pure ray
  where
    newC = dirVector dir .+. c

testModify :: Grid Char -> Grid Char
testModify g = runSTArray $ do
  ray <- thaw g
  writeArray ray (0, 0) 'h'
  return ray

gridLookup :: (Int, Int) -> Grid a -> Maybe a
gridLookup c g
  | inBounds g c = Just $ g A.! c
  | otherwise = Nothing

inBounds :: Grid a -> (Int, Int) -> Bool
inBounds g (x, y) =
  let ((x0, y0), (x1, y1)) = A.bounds g
   in A.inRange (x0, x1) x && A.inRange (y0, y1) y

dirVector :: Direction -> (Int, Int)
dirVector = \case
  N -> (0, -1)
  S -> (0, 1)
  E -> (1, 0)
  W -> (-1, 0)

printGrid :: Grid a -> (a -> Char) -> IO ()
printGrid g f = do
  let (_, (cols, rows)) = A.bounds g
  let ss = chunksOf (cols + 1) $ map f sorted
  putStrLn ""
  mapM_ putStrLn ss
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs =
      let (f, v) = splitAt n xs
       in f : chunksOf n v
    sorted = map snd $ sortBy (compareCoords `on` fst) $ A.assocs g

compareCoords :: (Int, Int) -> (Int, Int) -> Ordering
compareCoords (a, b) (c, d)
  | b == d = compare a c
  | otherwise = compare b d

data AStarNode where
  Node :: (Ord cost, Monoid cost) => cost -> Maybe AStarNode -> AStarNode

astar ::
  forall node cost.
  (Ord node, Ord cost, Monoid cost) =>
  -- | The start node.
  node ->
  -- | The goal node.
  node ->
  -- | The function to get the next nodes and their costs from a given node.
  --   The Maybe node is the node it came from, in case that's important for determining the cost
  ((Maybe node, node) -> [(node, cost)]) ->
  -- | The heuristic function to estimate the cost of going from a given node to
  --   the goal node.
  (node -> node -> cost) ->
  -- | Returns Nothing if no path found.
  --   Else returns Just (path cost, path as a list of nodes).
  Maybe (cost, [node])
astar startNode goalNode nextNodes heuristic =
  astar'
    (PQ.singleton (heuristic startNode goalNode) (startNode, mempty))
    Set.empty
    (Map.singleton startNode mempty)
    Map.empty
  where
    astar' ::
      -- \| The set of discovered nodes that need to be visited, stored
      --   in a min-priority queue prioritized by sum of costs of reaching to
      --   the nodes from the start node, and heuristic costs of reaching
      --   from the nodes to the goal node.
      PQ.MinPQueue cost (node, cost) ->
      -- \| The set of already visited nodes.
      Set.Set node ->
      -- \| The map of visited or discovered nodes to the currently known minimum
      --   costs from the start node to the nodes.
      Map.Map node cost ->
      -- \| The map of visited nodes to the previous nodes in the currently known
      --   best path from the start node.
      Map.Map node node ->
      -- \| Returns Nothing if no path found.
      --   Else returns Just (path cost, path as a list of nodes).
      Maybe (cost, [node])
    astar' !discovered !visited !minCosts tracks
      -- If the discovered set is empty then the search has failed. Return Nothing.
      | PQ.null discovered = Nothing
      -- If the current node is the goal node then return the current node cost and
      -- path to the current node constructed from the tracks.
      | node == goalNode = Just (cost, findPath tracks node)
      -- If the current node has already been visited then discard it and continue.
      | node `Set.member` visited =
          astar' discoveredSansCurrent visited minCosts tracks
      -- Else visit the current node and continue.
      | otherwise =
          let -- Add the current node to the visited set.
              fromNode = node `Map.lookup` tracks
              visited' = Set.insert node visited
              -- Find the successor nodes of the current node that have not been
              -- visited yet, along with their costs and heuristic costs.
              successors =
                [ (node', cost', heuristic node' goalNode)
                  | (node', nodeCost) <- nextNodes (fromNode, node), -- Get next nodes.
                    node' `Set.notMember` visited', -- Keep only unvisited ones.
                    let cost' = cost <> nodeCost, -- Cost of the next node.
                    -- Keep only unvisited nodes, or previously visited nodes now
                    -- discovered via less costly paths.
                    node' `Map.notMember` minCosts || cost' < minCosts Map.! node'
                ]

              -- Insert the successors in the discovered set.
              discovered' =
                foldl'
                  (\q (n, c, h) -> PQ.insert (c <> h) (n, c) q)
                  discoveredSansCurrent
                  successors
              -- Insert the successor costs in the minimum cost map.
              minCosts' = foldl' (\m (n, c, _) -> Map.insert n c m) minCosts successors
              -- Insert the tracks of the successors.
              tracks' = foldl' (\m (n, _, _) -> Map.insert n node m) tracks successors
           in -- Continue via recursion.
              astar' discovered' visited' minCosts' tracks'
      where
        -- Get (and delete) the node with minimum cost and its cost from the
        -- discovered set.
        ((_, (node, cost)), discoveredSansCurrent) = PQ.deleteFindMin discovered

    -- Construct the path of the given node from the start node using the
    -- recorded tracks.
    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (tracks Map.! node) ++ [node]
        else [node]

testGrid :: Grid Char
testGrid =
  stringToGrid id $
    unlines
      [ "91999",
        "91999",
        "91119",
        "99919",
        "99911"
      ]

res :: Maybe (Sum Int, [(Int, Int)])
res = astar start goal neighbors heuristic
  where
    start = (0, 0)
    goal = (4, 4)
    neighbors (_, c@(x, y)) =
      map (\c -> (c, Sum . digitToInt . fromJust $ gridLookup c testGrid)) $
        filter
          (inBounds testGrid)
          ( [(.+. c)]
              <*> [ (0, -1), -- up
                    (1, 0), -- right
                    (0, 1), -- down
                    (-1, 0) -- left
                  ]
          )
    heuristic (a, b) (c, d) = Sum $ abs (c - a) + abs (d - b)
