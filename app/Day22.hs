module Day22 where

import Control.Arrow ((&&&))
import Data.Bits (Bits (..))
import Data.Char (intToDigit)
import Data.Function (on)
import Data.List (intercalate, maximumBy)
import Data.Map qualified as M
import Data.Word (Word32)
import Debug.Trace (trace)
import Numeric (showIntAtBase)
import Text.Parsec (Parsec, newline, runParser, sepBy)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)

-- Function to generate all subsequences of length 'k' from a list 'xs'

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])

type Parser = Parsec String ()

type Input = [Word32]

shouldTrace :: Bool
shouldTrace = False

btrace :: String -> a -> a
btrace s a = if shouldTrace then trace s a else a

wordToBinary :: Word32 -> String
wordToBinary x = showIntAtBase 2 intToDigit x ""

pruneNum :: Word32
pruneNum = 16777215

mixPrune :: Word32 -> Word32 -> Word32
mixPrune x secret = (x `xor` secret) .&. pruneNum

f :: Word32 -> Word32
f secret =
  let shifted = (secret `shift` 6)
      mixPruned1 = mixPrune shifted secret
      backShifted = (mixPruned1 `shift` (-5))
      mixPruned2 = mixPrune backShifted mixPruned1
      bigShift = mixPruned2 `shift` 11
      mixPruned3 = mixPrune bigShift mixPruned2
   in btrace (intercalate "\n" . map wordToBinary $ [secret, shifted, mixPruned1, backShifted, mixPruned2, bigShift, mixPruned3]) mixPruned3

getSequence :: Int -> Word32 -> [Int]
getSequence n = reverse . diff . reverse . take (n + 1) . map ((`mod` 10) . fromIntegral) . iterate f
  where
    diff :: (Num a) => [a] -> [a]
    diff [] = []
    diff [_] = []
    diff (x : y : xs) = (x - y) : diff (y : xs)

nthSecret :: Int -> Word32 -> Word32
nthSecret n w = (!! n) $ iterate f w

part1 :: Input -> Integer
part1 = sum . map (fromIntegral . nthSecret 2000)

part2 :: Input -> M.Map [Int] Int
part2 = M.unionsWith (+) . map (convertToMap . subsequencesOfSize 4 . getSequence 2000)

convertToMap :: [[Int]] -> M.Map [Int] Int
convertToMap ls = M.fromList [(xs, 1) | xs <- ls]

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = sepBy (read <$> many1 digit) newline

main :: IO ()
main = readFile "inputs/day22input.txt" >>= print . (part1 &&& part2) . prepare