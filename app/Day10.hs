module Day10 where

import Control.Arrow ((&&&))
import Data.Array qualified as A
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Set qualified as S
import Text.Parsec (Parsec)

type Parser = Parsec String ()

type Coor = (Int, Int)

type Map = A.Array Coor Int

type GameState = (Coor, Map, S.Set Coor)

type Input = Map

step :: GameState -> [GameState]
step (c, m, s)
  | (m A.! c) == 9 = [(c, m, S.insert c s)]
  | otherwise = do
      let neighborsCoors = (.+.) <$> [c] <*> relativeNeighborCoors
      neighbor <- filter (not . (`S.member` s)) . filter neighborIsValid $ neighborsCoors
      step (neighbor, m, S.insert c s)
  where
    v = m A.! c
    relativeNeighborCoors = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    neighborIsValid :: Coor -> Bool
    neighborIsValid c1 = case m .?. c1 of
      Nothing -> False
      (Just i) -> i - v == 1

part1 :: Input -> Int
part1 a =
  let xs = A.assocs a
      validStarts = map fst . filter (\(_, v) -> v == 0) $ xs
      gamePaths = filter (not . null) . map (\c -> step (c, a, S.empty)) $ validStarts -- Each list of gamepaths includes one entry for each way the starting spot could have made it to a 9
      endCoorsForGamePaths = map (map (\(endCoor, _, _) -> endCoor)) gamePaths -- Filter down to the end coordinates for each of those "ways"
      uniqueEndsArrivedAt = map (length . nub) endCoorsForGamePaths -- If multiple paths had the same end coordiante, we only want to count it once, so nub and the length to determine the unique way of ends
   in sum uniqueEndsArrivedAt

part2 :: Input -> Int
part2 a = sum . map (length . (\(c, _) -> step (c, a, S.empty))) . filter (\(_, v) -> v == 0) $ A.assocs a

prepare :: String -> Input
prepare inp =
  let ls = [((x, y), v) | (y, ln) <- zip [0 ..] $ lines inp, (x, vc) <- zip [0 ..] ln, let v = digitToInt vc]
      xMin = 0
      yMin = 0
      xMax = maximum $ map (fst . fst) ls
      yMax = maximum $ map (snd . fst) ls
   in A.array ((xMin, yMin), (xMax, yMax)) ls

(.?.) :: A.Array Coor Int -> (Int, Int) -> Maybe Int
(.?.) a coor =
  let inBnds = A.inRange . A.bounds $ a
   in if not $ inBnds coor then Nothing else Just $ a A.! coor

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+. (c, d) = (a + c, b + d)

main :: IO ()
main = readFile "inputs/day10input.txt" >>= print . (part1 &&& part2) . prepare