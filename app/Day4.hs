module Day4 where

import Control.Arrow ((&&&))
import Data.List (isPrefixOf, transpose)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Text.Parsec (Parsec)

type Parser = Parsec String ()

type Input = [[Char]]

part1 :: Input -> Int
part1 inp =
  let vals = cols inp ++ rows inp ++ diagVals inp
   in sum $ map xmasCount vals

diagVals :: [String] -> [String]
diagVals inp =
  let x = diags inp ++ diags (map reverse inp)
   in map reverse x ++ x

xmasCount :: String -> Int
xmasCount [] = 0
xmasCount xss@(_ : xs)
  | "XMAS" `isPrefixOf` xss = 1 + xmasCount (drop 4 xss)
  | otherwise = xmasCount xs

type CoorMap = Map.Map (Int, Int) Char

part2 :: Input -> Int
part2 i = length $ filter (checkSpot minp) (Map.keys minp)
  where
    minp :: CoorMap
    minp = Map.fromList [((x, y), c) | (x, lin) <- zip [0 ..] i, (y, c) <- zip [0 ..] lin]
    coordinates =
      [ [(-1, -1), (0, 0), (1, 1)],
        [(1, 1), (0, 0), (-1, -1)],
        [(-1, 1), (0, 0), (1, -1)],
        [(1, -1), (0, 0), (-1, 1)]
      ] ::
        [[(Int, Int)]]
    (.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
    (a, b) .+. (c, d) = (a + c, b + d)
    checkSpot :: CoorMap -> (Int, Int) -> Bool
    checkSpot m c =
      let paths = ((.+.) <$> [c] <*>) <$> coordinates
          lookupSafe :: CoorMap -> [(Int, Int)] -> Maybe String
          lookupSafe cm = traverse (`Map.lookup` cm)
          strings = filter (== Just "MAS") $ map (lookupSafe m) paths
       in length strings == 2

rows :: [[a]] -> [[a]]
rows i = map reverse i ++ i

cols :: [[a]] -> [[a]]
cols = rows . transpose

diag :: [[a]] -> [a]
diag xs = catMaybes $ zipWith safeIndex xs [0 ..]

diagsDown :: [[a]] -> [[a]]
diagsDown [] = []
diagsDown xss@(_ : xs) = diag xss : diagsDown xs

diags :: [[a]] -> [[a]]
diags [] = []
diags xs
  | lenSum == 0 = []
  | otherwise = diagsDown xs ++ diagsAcross (map safeTail xs)
  where
    lenSum = sum $ map length xs

diagsAcross :: [[a]] -> [[a]]
diagsAcross [] = []
diagsAcross xs
  | lenSum == 0 = []
  | otherwise = diag xs : diagsAcross (map safeTail xs)
  where
    lenSum = sum $ map length xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) n = safeIndex xs (n - 1)

prepare :: String -> Input
prepare = lines

--     case runParser parser () "" inp of
--   (Left e) -> error $ show e
--   (Right res) -> res
--   where
--     parser :: Parser Input
--     parser = return $ Input ()

main :: IO ()
main = readFile "inputs/day4input.txt" >>= print . (part1 &&& part2) . prepare