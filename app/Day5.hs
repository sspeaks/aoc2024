module Day5 where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (groupBy, intersect, sortBy, (\\))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as Map
import Text.Parsec (Parsec, char, newline, runParser, sepBy)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)

type Parser = Parsec String ()

type Protocols = IntMap [Int]

type Sequence = [Int]

data Input = Puzzle Protocols [Sequence]

part1 :: Input -> Int
part1 (Puzzle protos seqs) = sum . map mid . filter (checkSequence protos) $ seqs

part2 :: Input -> Int
part2 (Puzzle protos seqs) = sum . map (mid . fixSequence protos) . filter (not . checkSequence protos) $ seqs

fixSequence :: Protocols -> [Int] -> [Int]
fixSequence _ [] = []
fixSequence protos items = go [] items
  where
    go :: [Int] -> [Int] -> [Int]
    go _ [] = []
    go excl xss@(x : xs)
      -- Need to move this item back to beginning of list
      | not (null (newExcl `intersect` xs)) = fixSequence protos ((newExcl `intersect` xs) ++ (xss \\ newExcl))
      | otherwise = x : go newExcl xs
      where
        newExcl = proceeds x protos ++ excl

checkSequence :: Protocols -> [Int] -> Bool
checkSequence protos = f []
  where
    f :: [Int] -> [Int] -> Bool
    f _ [] = True
    f excl (x : xs) =
      let newExcls = proceeds x protos ++ excl
       in (notElem x newExcls && f newExcls xs)

mid :: [Int] -> Int
mid xs = xs !! (length xs `div` 2)

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = do
      proto <- collapseIntoMap <$> many1 (parseTuple <* newline)
      _ <- newline
      seqs <- sepBy parseSeq newline
      return $ Puzzle proto seqs
    parseTuple :: Parser (Int, Int)
    parseTuple = (,) <$> (read <$> many1 digit <* char '|') <*> (read <$> many1 digit)
    parseNum :: Parser Int
    parseNum = read <$> many1 digit
    parseSeq :: Parser [Int]
    parseSeq = sepBy parseNum (char ',')

proceeds :: Int -> Protocols -> [Int]
proceeds n ps = Map.keys $ Map.filter (n `elem`) ps

collapseIntoMap :: [(Int, Int)] -> Protocols
collapseIntoMap ls =
  let f :: [(Int, Int)] -> (Int, [Int])
      f [] = error "Impossible"
      f xs = let item = fst $ head xs in (item,) $ map snd xs
   in Map.fromList . map f . groupBy ((==) `on` fst) $ sortBy (compare `on` fst) ls

main :: IO ()
main = readFile "inputs/day5input.txt" >>= print . (part1 &&& part2) . prepare
