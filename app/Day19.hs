{-# LANGUAGE BangPatterns #-}

module Day19 where

import Control.Arrow ((&&&))
import Control.Monad.State qualified as ST
import Data.List (isPrefixOf)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Text.Parsec (Parsec, alphaNum, many1, runParser, sepBy, spaces, string)

data Color = White | Blue | Black | Red | Green deriving (Show, Eq, Generic, Ord)

type TargetDesign = [Color]

type TowelPattern = [Color]

type Parser = Parsec String ()

type Input = ([TowelPattern], [TargetDesign])

part1 :: Input -> Int
part1 (patts, dess) = length . filter (> 0) . map (solutionsPerDesign patts) $ dess

part2 :: Input -> Int
part2 (patts, dess) = sum . map (solutionsPerDesign patts) $ dess

solutionsPerDesign :: [TowelPattern] -> TargetDesign -> Int
solutionsPerDesign patts design = ST.evalState (f design) M.empty
  where
    f :: TargetDesign -> ST.State (M.Map TargetDesign Int) Int
    f [] = return 1
    f des = do
      !seen <- ST.gets (M.lookup des)
      case seen of
        (Just v) -> return v
        Nothing -> do
          let !workingPatts = filter (`isPrefixOf` des) patts
          !viableSuffixCount <- sum <$> mapM (\p -> f $ drop (length p) des) workingPatts
          ST.modify (M.insert des viableSuffixCount)
          return viableSuffixCount

type Solution = [TowelPattern]

-- Brute force solution
f' :: [TowelPattern] -> TargetDesign -> [Solution]
f' patts des =
  [ patt : rest
    | patt <- patts,
      patt `isPrefixOf` des,
      let tailOf = drop (length patt) des,
      rest <- f' patts tailOf
  ]

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