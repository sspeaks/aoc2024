{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Day22 where

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.Bits (Bits (..))
import Data.Char (intToDigit)
import Data.Function (on)
import Data.List (intercalate, maximumBy, tails)
import Data.Map qualified as M
import Data.Word (Word32)
import Debug.Trace (trace)
import GHC.Exts (groupWith)
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

seqe :: (Integral a) => [a] -> [a]
seqe [] = []
seqe xs = scanl (\_ (a, b) -> b - a) 0 (zip xs' (tail xs'))
  where
    xs' = map (`mod` 10) xs

part1 :: Input -> Integer
part1 = sum . map (fromIntegral . nthSecret 2000)

part2 :: Input -> ([Int], Int)
part2 _ = maximumBy (compare `on` snd) $ hylo trieCoalg trieAlg ls -- [[1,2,3],[1,2,4],[1,2,4], [2,2,4]]
  where
    ls = filter ((== 4) . length) . map (take 4) . tails . seqe $ map (\n -> fromIntegral $ nthSecret n 123) [0 .. 1999]

-- Going to try my hand at a hylomorphism....
newtype Fix f = In {out :: f (Fix f)}

data TrieF b = TrieF [(Int, Int, b)] deriving (Show, Functor)

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg

hylo :: (Functor f) => (a -> f a) -> (f b -> b) -> a -> b
hylo coalg alg = go where go = alg . fmap go . coalg

trieCoalg :: [[Int]] -> TrieF [[Int]]
trieCoalg [] = error "shouldn't happen"
trieCoalg xs
  | null (head xs) = TrieF []
  | otherwise = TrieF . map toTruple . groupWith head $ xs
  where
    toTruple vs =
      let c = head . head $ vs
          l = length vs
       in (c, l, map tail vs)

trieAlg :: TrieF [([Int], Int)] -> [([Int], Int)]
trieAlg (TrieF []) = []
trieAlg (TrieF v) = concatMap (\(item, count, prevs) -> go item count prevs) v
  where
    go :: Int -> Int -> [([Int], Int)] -> [([Int], Int)]
    go item count [] = [([item], count)]
    go item _ xs = map (first (item :)) xs

data Trie = Trie [(Int, Int, Trie)] deriving (Show)

baseTrieFToTrie :: Fix TrieF -> Trie
baseTrieFToTrie = cata alg
  where
    alg (TrieF []) = Trie []
    alg (TrieF xs) = Trie xs

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