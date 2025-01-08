{-# LANGUAGE LambdaCase #-}

module Day15 where

import Control.Arrow ((&&&))
import Data.Array qualified as A
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)
import Grid (Direction (..), Grid, dirVector, findInGrid, gridLookup, moveCell, printGrid, stringToGrid, (.+.))
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

class HasCellValue a where
  getCell :: a -> Cell
  toSpot :: Cell -> a

instance HasCellValue Cell where
  getCell :: Cell -> Cell
  getCell = id
  toSpot :: Cell -> Cell
  toSpot = id

instance HasCellValue (Cell, Maybe (Int, Int)) where
  getCell :: (Cell, Maybe (Int, Int)) -> Cell
  getCell = fst
  toSpot :: Cell -> (Cell, Maybe (Int, Int))
  toSpot c = (c, Nothing)

canMoveDir :: (HasCellValue a) => Grid a -> (Int, Int) -> Direction -> Bool
canMoveDir g c d = case getCell <$> gridLookup newC g of
  Nothing -> False
  Just v
    | v == Wall -> False
    | v == Box -> canMoveDir g newC d
    | otherwise -> True
  where
    newC = c .+. dirVector d

moveDir :: (HasCellValue a) => Grid a -> (Int, Int) -> Direction -> (Grid a, (Int, Int))
moveDir g c d
  | not $ canMoveDir g c d = (g, c)
  | otherwise = case getCell <$> gridLookup newC g of
      Nothing -> error "Shouldn't happen... we've already check the move"
      Just Box ->
        let (newG, _) = moveDir g newC d
         in (fromJust $ moveCell newG c d (const (toSpot Empty)), newC)
      _ -> (fromJust $ moveCell g c d (const (toSpot Empty)), newC)
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
part2 (g, _) =
  let newG = gridMorph g
      v = (unsafePerformIO $ printGrid newG (cellToc . fst))
   in v

-- Each cell is a value and a Maybe indicating whether they have a neighbor or not
gridMorph :: Grid Cell -> Grid (Cell, Maybe (Int, Int))
gridMorph g = A.array ((0, 0), (newWidth - 1, newHeight - 1)) $ A.assocs g >>= f
  where
    (w, h) = snd $ A.bounds g
    (newWidth, newHeight) = ((w + 1) * 2, h + 1)
    f ((x, y), Box) = [((x * 2, y), (Box, Just (x * 2 + 1, y))), ((x * 2 + 1, y), (Box, Just (x * 2, y)))]
    f ((x, y), Wall) = [((x * 2, y), (Wall, Nothing)), ((x * 2 + 1, y), (Wall, Nothing))]
    f ((x, y), Robot) = [((x * 2, y), (Robot, Nothing)), ((x * 2 + 1, y), (Empty, Nothing))]
    f ((x, y), Empty) = [((x * 2, y), (Empty, Nothing)), ((x * 2 + 1, y), (Empty, Nothing))]

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