module Day3 where

import Control.Arrow ((&&&))
import Text.Parsec (char, digit, many1, parserFail, runParser, anyChar, (<|>), try, Parsec, getState, putState)
import Text.Parsec.Char (string)

type Parser = Parsec String Bool

newtype Input = Input [Instruction] deriving (Show)

data Instruction = Mul Bool Int Int deriving (Show)

evalInstruction :: Instruction -> Int
evalInstruction (Mul True a b) = a * b
evalInstruction (Mul False _ _) = 0

part1 :: Input -> Int
part1 (Input is) = sum $ evalInstruction . (\(Mul _ a b) -> Mul True a b) <$> is

part2 :: Input -> Int
part2 (Input is) = sum $ evalInstruction <$> is

prepare :: String -> Input
prepare inp = case runParser parser True "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = Input <$> many1 (try parseChunk)
    parseChunk :: Parser Instruction
    parseChunk = (try parseDo <|> try parseDont <|> return ()) *> (try parseMul <|> (anyChar *> parseChunk))
    parseDo :: Parser ()
    parseDo = string "do()" >> putState True
    parseDont :: Parser ()
    parseDont = string "don't()" >> putState False
    parseMul :: Parser Instruction
    parseMul = do 
        s <- getState
        Mul s <$> (string "mul(" *> parseNum <* char ',') <*> parseNum <* char ')'
    parseNum :: Parser Int
    parseNum = do
      i <- read <$> many1 digit
      if i > 999 then parserFail "number larger than 3 digits" else return i

main :: IO ()
main = readFile "inputs/day3input.txt" >>= print . (part1 &&& part2) . prepare