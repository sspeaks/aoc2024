module Day23 where

import Clique (getMaximalCliques, getTriangles)
import Control.Arrow ((&&&))
import Data.Array qualified as A
import Data.Function (on)
import Data.Graph qualified as G
import Data.List (intercalate, maximumBy, sort)
import Data.Map qualified as M
import Text.Parsec (Parsec, alphaNum, char, many1, runParser)
import Text.Parsec.Char (spaces)

type Parser = Parsec String ()

type Computer = String

data Connection = Conn Computer Computer deriving (Show)

data Trio = Trio Computer Computer Computer deriving (Show)

instance Eq Trio where
  (==) :: Trio -> Trio -> Bool
  (Trio a b c) == (Trio d e f) = sort [a, b, c] == sort [d, e, f]

type Input = (G.Graph, G.Vertex -> (String, String, [String]), String -> Maybe G.Vertex)

vertexToComputer :: (G.Vertex -> (String, String, [String])) -> G.Vertex -> Computer
vertexToComputer nfv i = (\(a, _, _) -> a) $ nfv i

containsT :: Trio -> Bool
containsT (Trio a b c) = any test [a, b, c]
  where
    test [] = False
    test ('t' : _) = True
    test _ = False

part1 :: Input -> Int
part1 (g, nfv, _) = length $ filter containsT trianglesOfComputerTrios
  where
    trianglesOfComputerTrios = map convertToComputerTrio (getTriangles g)
    convertToComputerTrio [a, b, c] = Trio (vertexToComputer nfv a) (vertexToComputer nfv b) (vertexToComputer nfv c)
    convertToComputerTrio _ = error "anything more than 3 elems should never happen"

part2 :: Input -> String
part2 (g, nfv, _) =
  let cliques = getMaximalCliques f nodes
   in intercalate "," . sort . map (vertexToComputer nfv) $ maximumBy (compare `on` length) cliques
  where
    nodes = A.indices g
    f a b = b `elem` (g A.! a)

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = do
      conns <- many1 (parseConn <* spaces)
      let vertices = M.assocs $ M.fromListWith (<>) $ conns >>= \(Conn a b) -> [(a, [b]), (b, [a])]
      return $ G.graphFromEdges $ map (\(a, b) -> (a, a, b)) vertices
    parseConn :: Parser Connection
    parseConn = Conn <$> (many1 alphaNum <* char '-') <*> many1 alphaNum

main :: IO ()
main = readFile "inputs/day23input.txt" >>= print . (part1 &&& part2) . prepare