{-# LANGUAGE LambdaCase #-}

module Day15 where

import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Grid (Direction (..), Grid, dirVector, findInGrid, gridLookup, moveCell, stringToGrid, (.+.))
import Text.Parsec (Parsec, many1, manyTill, runParser, spaces, string, try)
import Text.Parsec.Char (anyChar)

data Cell = Robot | Wall | Box | Empty deriving (Show, Eq)

cToCell :: Char -> Cell
cToCell = \case
  '#' -> Wall
  '@' -> Robot
  'O' -> Box
  '.' -> Empty
  c -> error ("unknown cell type " <> show c)

cellToc :: Cell -> Char
cellToc = \case
  Wall -> '#'
  Robot -> '@'
  Box -> 'O'
  Empty -> '.'

cToDir :: Char -> Direction
cToDir = \case
  '^' -> N
  '>' -> E
  '<' -> W
  'v' -> S
  c -> error ("unknown direction " <> show c)

type Parser = Parsec String ()

type Input = (Grid Cell, [Direction])

canMoveDir :: Grid Cell -> (Int, Int) -> Direction -> Bool
canMoveDir g c d = case gridLookup newC g of
  Nothing -> False
  Just v
    | v == Wall -> False
    | v == Box -> canMoveDir g newC d
    | otherwise -> True
  where
    newC = c .+. dirVector d

moveDir :: Grid Cell -> (Int, Int) -> Direction -> (Grid Cell, (Int, Int))
moveDir g c d
  | not $ canMoveDir g c d = (g, c)
  | otherwise = case gridLookup newC g of
      Nothing -> error "Shouldn't happen... we've already check the move"
      Just Box ->
        let (newG, _) = moveDir g newC d
         in (fromJust $ moveCell newG c d (const Empty), newC)
      _ -> (fromJust $ moveCell g c d (const Empty), newC)
  where
    newC = c .+. dirVector d

part1 :: Input -> Int
part1 (g, dirs) =
  let (newG, _) = foldl go (g, robotPos) dirs
      v = sum . map score $ findInGrid (== Box) newG
   in v
  where
    --    in unsafePerformIO (printGrid newG cellToc >> v)

    robotPos = head $ findInGrid (== Robot) g
    go (grid, pos) = moveDir grid pos
    score (x, y) = 100 * y + x

part2 :: Input -> ()
part2 _ = ()

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = (,) <$> parseGrid <*> parseDirs
    parseGrid :: Parser (Grid Cell)
    parseGrid = stringToGrid cToCell <$> manyTill anyChar (try $ string "\n\n") <* spaces
    parseDirs :: Parser [Direction]
    parseDirs = map cToDir <$> many1 (anyChar <* spaces)

main :: IO ()
main = readFile "inputs/day15input.txt" >>= print . (part1 &&& part2) . prepare