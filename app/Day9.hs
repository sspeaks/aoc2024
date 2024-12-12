module Day9 where

import Control.Arrow ((&&&))
import Control.Monad.ST (ST)
import Data.Array (Array)
import Data.Array qualified as A
import Data.Array.ST (STArray, runSTArray)
import Data.Array.ST qualified as A
import Data.Char (digitToInt)
import Text.Parsec (Parsec, digit, getState, many1, putState, runParser)

type Parser = Parsec String Int

data DiskSpot
  = Free {freeSize :: Int}
  | File {fileId :: Int, fileSize :: Int}
  deriving (Show, Eq)

type Disk = Array Int DiskSpot

type Input = Disk

part1 :: Input -> Input
part1 inp = runSTArray $ do
  mutArray <- A.thaw inp :: ST s (STArray s Int DiskSpot)
  undefined

part2 :: Input -> ()
part2 _ = ()

prepare :: String -> Input
prepare inp = case runParser parser 0 "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = do
      ls <- filter (/= Free 0) <$> many1 fileOrFree
      let l = length ls
      return $ A.listArray (0, l - 1) ls
    fileOrFree :: Parser DiskSpot
    fileOrFree = do
      ind <- getState
      f <- if even ind then File (ind `div` 2) . digitToInt <$> digit else Free . digitToInt <$> digit
      putState (ind + 1)
      return f

main :: IO ()
main = readFile "inputs/day9input.txt" >>= print . (part1 &&& part2) . prepare