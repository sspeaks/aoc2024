{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Day19 where

import Control.Arrow ((&&&))
import Data.List (isPrefixOf)
import Data.MemoTrie (HasTrie (..), Reg, enumerateGeneric, memo, trieGeneric, untrieGeneric, (:->:))
import GHC.Generics (Generic)
import Text.Parsec (Parsec, alphaNum, many1, runParser, sepBy, spaces, string)

data Color = White | Blue | Black | Red | Green deriving (Show, Eq, Generic, Ord)

instance HasTrie Color where
  newtype Color :->: b = ColorTrie {unColorTrie :: Reg Color :->: b}
  trie :: (Color -> b) -> Color :->: b
  trie = trieGeneric ColorTrie
  untrie :: (Color :->: b) -> Color -> b
  untrie = untrieGeneric unColorTrie
  enumerate :: (Color :->: b) -> [(Color, b)]
  enumerate = enumerateGeneric unColorTrie

type TargetDesign = [Color]

type TowelPattern = [Color]

type Parser = Parsec String ()

type Input = ([TowelPattern], [TargetDesign])

part1 :: Input -> Int
part1 (patts, dess) = length . filter (> 0) . map (memoSolve patts) $ dess

part2 :: Input -> Int
part2 (patts, dess) = sum . map (memoSolve patts) $ dess

-- Brute force solution
memoSolve :: [TowelPattern] -> TargetDesign -> Int
memoSolve patts = memo $ \case
  [] -> 1
  des ->
    let workingPatts = filter (`isPrefixOf` des) patts
        viableSuffixCount = sum $ map (\p -> memoSolve patts $ drop (length p) des) workingPatts
     in viableSuffixCount

-- Solution to get all solutions... very space inefficient
-- [ patt : rest
--   | patt <- patts,
--     patt `isPrefixOf` des,
--     let tailOf = drop (length patt) des,
--     n <- f' patts tailOf
-- ]

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = (,) <$> parseTowelPatterns <*> parseTargetDesigns
    parseTowelPatterns :: Parser [TowelPattern]
    parseTowelPatterns = sepBy parseColorSeq (string ", ") <* spaces
    parseColorSeq :: Parser TowelPattern
    parseColorSeq = many1 (getColor <$> alphaNum) <* spaces
    parseTargetDesigns :: Parser [TargetDesign]
    parseTargetDesigns = many1 parseColorSeq
    getColor :: Char -> Color
    getColor 'r' = Red
    getColor 'w' = White
    getColor 'u' = Blue
    getColor 'b' = Black
    getColor 'g' = Green
    getColor c = error $ "Char " <> [c] <> " doesn't exist as a color"

main :: IO ()
main = readFile "inputs/day19input.txt" >>= print . (part1 &&& part2) . prepare