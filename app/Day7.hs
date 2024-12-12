module Day7 where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.List (inits, tails)
import Text.Parsec (Parsec, char, digit, many1, newline, runParser, sepBy, string)

type Parser = Parsec String ()

data Numbers = Numbers {goal :: Integer, sources :: [Integer]} deriving (Show)

type Input = [Numbers]

data Op = Multiply | Add | Cat

data Expr = Val Integer | Tree Op Expr Expr

evalExpr :: Expr -> Maybe Integer
evalExpr (Val x) = Just x
evalExpr (Tree o l r) = case o of
  Multiply -> liftM2 (*) left right
  Add -> liftM2 (+) left right
  Cat -> liftM2 catInts left right
  where
    left = evalExpr l
    right = evalExpr r
    catInts :: Integer -> Integer -> Integer
    catInts a b = read $ show a <> show b

splits :: [a] -> [([a], [a])]
splits xs = init . tail $ zip (inits xs) (tails xs)

combos :: Expr -> Expr -> [Expr]
combos l r = Tree <$> [Add, Multiply] <*> [l] <*> [r]

catCombos :: Expr -> Expr -> [Expr]
catCombos l r = Tree <$> [Add, Multiply, Cat] <*> [l] <*> [r]

-- All possible parenthesizations of the numbers, instead of just left-to-right
trees :: [Integer] -> [Expr]
trees [x] = [Val x]
trees xs = do
  (f, s) <- splits xs
  l <- trees f
  r <- trees s
  combos l r

leftTrees :: [Integer] -> (Expr -> Expr -> [Expr]) -> [Expr]
leftTrees [x] _ = [Val x]
leftTrees xs fn = do
  let (f, s) = last $ splits xs
  l <- leftTrees f fn
  r <- leftTrees s fn
  fn l r

checkNumbers :: Numbers -> (Expr -> Expr -> [Expr]) -> Bool
checkNumbers (Numbers g ss) fn = any ((== Just g) . evalExpr) (leftTrees ss fn)

part1 :: Input -> Integer
part1 = sum . map goal . filter (`checkNumbers` combos)

part2 :: Input -> Integer
part2 = sum . map goal . filter (`checkNumbers` catCombos)

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = parseNumbers `sepBy` newline
    parseInteger :: Parser Integer
    parseInteger = read <$> many1 digit
    parseNumbers :: Parser Numbers
    parseNumbers = Numbers <$> (parseInteger <* string ": ") <*> parseInteger `sepBy` char ' '

main :: IO ()
main = readFile "inputs/day7input.txt" >>= print . (part1 &&& part2) . prepare