{-# LANGUAGE LambdaCase #-}

module Day19 where

import Control.Arrow ((&&&))
import Data.List (isPrefixOf)
import Data.MemoTrie (memo)
import Text.Parsec (Parsec, many1, oneOf, runParser, sepBy, spaces, string)

type TargetDesign = [Char]

type TowelPattern = [Char]

type Parser = Parsec String ()

type Input = ([TowelPattern], [TargetDesign])

part1 :: Input -> Int
part1 (patts, dess) = length . filter (> 0) . map (memoSolve patts) $ dess

part2 :: Input -> Int
part2 (patts, dess) = sum . map (memoSolve patts) $ dess

memoSolve :: [TowelPattern] -> TargetDesign -> Int
memoSolve patts = memo $ \case
  [] -> 1
  des ->
    sum
      [ n
        | patt <- patts,
          patt `isPrefixOf` des,
          n <- [memoSolve patts (drop (length patt) des)]
      ]

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser =
      (,)
        <$> (sepBy parseColors (string ", ") <* spaces)
        <*> (many1 parseColors <* spaces)
    parseColors :: Parser TowelPattern
    parseColors = many1 (oneOf "rwubg") <* spaces

main :: IO ()
main = readFile "inputs/day19input.txt" >>= print . (part1 &&& part2) . prepare