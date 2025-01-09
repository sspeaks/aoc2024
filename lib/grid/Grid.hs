{-# LANGUAGE LambdaCase #-}

module Grid where

import Data.Array.IArray qualified as A
import Data.Array.ST (STArray, modifyArray', readArray, runSTArray, thaw, writeArray)
import Data.Function (on)
import Data.List (sortBy)
import Data.Set qualified as S

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

aStar ::
  (Ord b, Monoid b) =>
  a -> -- Starting location
  (a -> [(a, b)]) -> -- Given a node, give it's possible moves and their costs
  (a -> b) -> -- Heuristic function that gives, for example, distance from start to
  AStarNode
aStar start neighbors heuristicDistance = go start S.empty
  where
    go :: a -> S.Set a -> AStarNode
    go curr s = undefined