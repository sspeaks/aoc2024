module Template where

import Control.Arrow ((&&&))
import Text.Parsec (Parsec, runParser)

type Parser = Parsec String ()

type Input = ()

part1 :: Input -> ()
part1 _ = ()

part2 :: Input -> ()
part2 _ = ()

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = return ()

main :: IO ()
main = readFile "inputs/day5input.txt" >>= print . (part1 &&& part2) . prepare