module Day19 where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq, rseq)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Text.Parsec (Parsec, alphaNum, anyChar, many1, runParser, sepBy, spaces, string)

data Color = White | Blue | Black | Red | Green deriving (Show, Eq, Generic)

instance NFData Color

type TargetDesign = [Color]

type TowelPattern = [Color]

type Parser = Parsec String ()

type Input = ([TowelPattern], [TargetDesign])

part1 :: Input -> Int
part1 inp = let !res = f inp in length . filter (not . null) $ res
  where
    f (!patts, !designs) = map (f' patts) designs
    f' :: [TowelPattern] -> TargetDesign -> [[TowelPattern]]
    f' [] _ = error "should never have 0 patterns"
    f' _ [] = []
    f' patts design =
      let a = mapMaybe (g design) patts 
          x =
            mapMaybe
              mapped
              a
       in map head x
      where
        mapped :: (TowelPattern, TargetDesign) -> Maybe [[TowelPattern]]
        mapped (p, []) = let v = Just [[p]] in trace (show v) v
        mapped (p, d) = case f' patts d of
          [] -> trace ( "Nothing") Nothing
          workings -> let v = Just $ (:) <$> [p] <*> workings in trace (show v) v
    g :: TargetDesign -> TowelPattern -> Maybe (TowelPattern, TargetDesign)
    g [] _ = Nothing
    g d p
      | p `isPrefixOf` d = let !v = Just (p, drop (length p) d) in trace (show v) v
      | otherwise = Nothing

-- length . filter (not . null) $

part2 :: Input -> ()
part2 _ = ()

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