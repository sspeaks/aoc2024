module Day16 where

import Control.Arrow ((&&&))
import Data.Monoid (Sum (..))
import Grid (Direction (..), Grid, astar, findInGrid, gridLookup, inBounds, stringToGrid, (.+.))

type Input = Grid Char

getDir :: (Int, Int) -> (Int, Int) -> Direction
getDir (toX, toY) (fromX, fromY) = case (toX - fromX, toY - fromY) of
  (1, 0) -> E
  (-1, 0) -> W
  (0, -1) -> N
  (0, 1) -> S
  _ -> error "nodes should only be at most 1 manhattan distance apart"

getMoveCost :: Direction -> (Int, Int) -> (Int, Int) -> Sum Int
getMoveCost d from to =
  let moveDir = getDir to from
   in Sum $ case (d, moveDir) of
        (N, N) -> 1
        (N, W) -> 1001
        (N, S) -> 2001
        (N, E) -> 1001
        (W, N) -> 1001
        (W, W) -> 1
        (W, S) -> 1001
        (W, E) -> 2001
        (S, N) -> 2001
        (S, W) -> 1001
        (S, S) -> 1
        (S, E) -> 1001
        (E, N) -> 1001
        (E, W) -> 2001
        (E, S) -> 1001
        (E, E) -> 1

-- "dir" in this case is the direction the node is facing
neighCostsWithDir :: Grid Char -> (Int, Int) -> Direction -> [((Int, Int), Sum Int)]
neighCostsWithDir grid node dir =
  let validNeighs = filter (\c -> inBounds grid c && c `gridLookup` grid /= Just '#') ([(.+. node)] <*> neighborVectors)
   in map (\c -> (c, getMoveCost dir node c)) validNeighs
  where
    neighborVectors =
      [ (0, 1),
        (0, -1),
        (1, 0),
        (-1, 0)
      ]

heur :: (Int, Int) -> (Int, Int) -> Sum Int
heur (a, b) (c, d) = Sum $ abs (c - a) + abs (d - b)

part1 :: Input -> Maybe (Sum Int, [(Int, Int)])
part1 inp = astar startNode goalNode neighCosts heur
  where
    startNode = head $ findInGrid (== 'S') inp
    goalNode = head $ findInGrid (== 'E') inp
    neighCosts :: (Maybe (Int, Int), (Int, Int)) -> [((Int, Int), Sum Int)]
    neighCosts (Nothing, node) = neighCostsWithDir inp node E
    neighCosts (Just from, node) = neighCostsWithDir inp node (getDir node from)

part2 :: Input -> ()
part2 _ = ()

prepare :: String -> Input
prepare = stringToGrid id

main :: IO ()
main = readFile "inputs/day16input.txt" >>= print . (part1 &&& part2) . prepare