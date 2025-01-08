module Day14 where

import Control.Monad (forM_)
import Data.Char (intToDigit)
import Data.List (group, intercalate, sort, sortBy)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Linear (V2 (V2))
import Text.Parsec (Parsec, char, digit, many1, runParser, spaces, string, try, (<|>))

type Parser = Parsec String ()

data Elements = Elements
  { position :: V2 Int,
    velocity :: V2 Int
  }
  deriving (Show)

type Input = [Elements]

width :: Int
width = 101

height :: Int
height = 103

move :: Elements -> Elements
move els@(Elements pos vel) =
  let (V2 x1 y1) = pos + vel
      newPos = V2 (x1 `mod` width) (y1 `mod` height)
   in els {position = newPos}

(.*.) :: V2 Int -> Int -> V2 Int
(V2 a b) .*. n = V2 (a * n) (b * n)

moveBotsForSeconds :: [Elements] -> Int -> [Elements]
moveBotsForSeconds bots seconds = map (moveBotForSeconds seconds) bots
  where
    moveBotForSeconds :: Int -> Elements -> Elements
    moveBotForSeconds s el@(Elements _ v) =
      let (Elements newPos _) = move (el {velocity = v .*. s})
       in el {position = newPos}

whichQuad :: Elements -> Maybe Int
whichQuad (Elements (V2 x y) _) =
  let topHalf = y < height `div` 2
      middleHeight = y == height `div` 2
      leftHalf = x < width `div` 2
      middleWidth = x == width `div` 2
   in case (topHalf, leftHalf, middleHeight, middleWidth) of
        (_, _, True, _) -> Nothing
        (_, _, _, True) -> Nothing
        (True, True, _, _) -> Just 1
        (True, False, _, _) -> Just 2
        (False, True, _, _) -> Just 3
        (False, False, _, _) -> Just 4

renderElements :: [Elements] -> [Char]
renderElements els =
  let elsMap = (M.fromListWith (+) [((x, y), 1) | (Elements (V2 x y) _) <- els]) `M.union` M.fromList [((x, y), 0) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
   in map f . sortBy order $ M.assocs elsMap
  where
    f :: ((Int, Int), Int) -> Char
    f (_, c)
      | c == 0 = '.'
      | otherwise = intToDigit c

order :: ((Int, Int), b1) -> ((Int, Int), b2) -> Ordering
order ((x1, y1), _) ((x2, y2), _)
  | y2 * width + x2 > y1 * width + x1 = LT
  | y1 == y2 && x1 == x2 = EQ
  | otherwise = GT

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (f, l) = splitAt n xs
   in f : chunksOf n l

part1 :: Input -> Int
part1 bots = product . map length . group . sort . mapMaybe whichQuad $ moveBotsForSeconds bots 100

-- Day18 : ([Elements {position = V2 3 5, velocity = V2 3 (-3)}], ())

part2 :: Input -> [(Int, String)]
part2 = (: []) . (!! 7051) . take 100000 . zip [1 ..] . map renderBots . iterate (map move)

renderBots :: [Elements] -> [Char]
renderBots = intercalate "\n" . chunksOf width . renderElements

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = many1 parseElements
    parseElements :: Parser Elements
    parseElements = Elements <$> (string "p=" *> parseV2) <*> (string " v=" *> parseV2) <* spaces
    parseV2 :: Parser (V2 Int)
    parseV2 = V2 <$> (parseNum width <* char ',') <*> parseNum height
    parseNum :: Int -> Parser Int
    parseNum n = do
      sign <- try (string "-") <|> return ""

      let modifier = if sign == "-" then -1 else 1
      num <- (read <$> many1 digit :: Parser Int)
      return $ case modifier of
        (-1) -> n - num
        1 -> num
        _ -> error "shouldnt hit"

main :: IO ()
main =
  putStrLn "\n"
    >> readFile "inputs/day14input.txt"
    >>= \inp -> do
      let x = prepare inp
      forM_ (part2 x) $ \(i, s) -> do
        print i
        putStrLn s

-- threadDelay (1000000 `div` 5)
