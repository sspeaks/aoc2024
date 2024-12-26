module Day17 where

import Control.Arrow ((&&&))
import Control.Monad.State qualified as S
import Data.Array qualified as A
import Data.Bits (Bits (shift, xor, (.|.)))
import Debug.Trace (trace)
import Text.Parsec (Parsec, char, digit, many1, newline, runParser, sepBy, string)
import Data.Maybe (catMaybes)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

type Parser = Parsec String ()

data ProgramState = ProgramState
  { instructionPointer :: Int,
    registerA :: Integer,
    registerB :: Integer,
    registerC :: Integer,
    program :: A.Array Int OpWithOperand,
    output :: [Integer]
  }
  deriving (Show)

data Op
  = Adv
  | Bxl
  | Bst
  | Jnz
  | Bxc
  | Out
  | Bdv
  | Cdv
  deriving (Show)

shouldTrace :: Bool
shouldTrace = False

tracef :: String -> a -> a
tracef s v = if shouldTrace then trace s v else v

data OpWithOperand = Op Op Integer deriving (Show)

type Input = ProgramState

runProgram :: S.State ProgramState ()
runProgram = do
  (ProgramState {instructionPointer, program}) <- S.get

  let inBounds = tracef (show instructionPointer) $ A.inRange (A.bounds program) instructionPointer
  if not inBounds
    then return ()
    else stepProgram >> runProgram

stepProgram :: S.State ProgramState ()
stepProgram = do
  (ProgramState {instructionPointer, program}) <- S.get
  let opWa = program A.! instructionPointer
  execOp opWa

execOp :: OpWithOperand -> S.State ProgramState ()
execOp op = case op of
  Op Adv n -> tracef ("running adv " <> show op) $ adv n
  Op Bxl n -> tracef ("running bxl " <> show op) $ bxl n
  Op Bst n -> tracef ("running bst " <> show op) $ bst n
  Op Jnz n -> tracef ("running jnz " <> show op) $ jnz n
  Op Bxc n -> tracef ("running bxc " <> show op) $ bxc n
  Op Out n -> tracef ("running out " <> show op) $ out n
  Op Bdv n -> tracef ("running bdv " <> show op) $ bdv n
  Op Cdv n -> tracef ("running cdv " <> show op) $ cdv n

comboOp :: Integer -> S.State ProgramState Integer
comboOp i = do
  (ProgramState _ a b c _ _) <- S.get
  return $ case i of
    n | n `elem` [0 .. 3] -> n
    4 -> a
    5 -> b
    6 -> c
    7 -> error "invalid combo operand"
    _ -> error "unknown combo operand"

adv :: Integer -> S.State ProgramState ()
adv op = do
  state@(ProgramState ip a _ _ _ _) <- S.get
  combo <- comboOp op
  let result = a `div` (2 ^ combo)
  S.put (state {registerA = result, instructionPointer = ip + 1})

bxl :: Integer -> S.State ProgramState ()
bxl op = do
  state@(ProgramState ip _ b _ _ _) <- S.get
  S.put (state {registerB = b `xor` op, instructionPointer = ip + 1})

bst :: Integer -> S.State ProgramState ()
bst op = do
  state@(ProgramState {instructionPointer}) <- S.get
  combo <- comboOp op
  S.put (state {registerB = combo `mod` 8, instructionPointer = instructionPointer + 1})

jnz :: Integer -> S.State ProgramState ()
jnz op = do
  state@(ProgramState ip a _ _ _ _) <- S.get
  case a of
    0 -> S.put (state {instructionPointer = ip + 1})
    _ -> S.put (state {instructionPointer = fromIntegral op})

bxc :: Integer -> S.State ProgramState ()
bxc _ = do
  state@(ProgramState {registerB, registerC, instructionPointer}) <- S.get
  S.put (state {registerB = registerB `xor` registerC, instructionPointer = instructionPointer + 1})

out :: Integer -> S.State ProgramState ()
out op = do
  state@(ProgramState {output, instructionPointer}) <- S.get
  combo <- comboOp op
  S.put (state {output = combo `mod` 8 : output, instructionPointer = instructionPointer + 1})

bdv :: Integer -> S.State ProgramState ()
bdv op = do
  state@(ProgramState ip a _ _ _ _) <- S.get
  combo <- comboOp op
  let result = a `div` (2 ^ combo)
  S.put (state {registerB = result, instructionPointer = ip + 1})

cdv :: Integer -> S.State ProgramState ()
cdv op = do
  state@(ProgramState ip a _ _ _ _) <- S.get
  combo <- comboOp op
  let result = a `div` (2 ^ combo)
  S.put (state {registerC = result, instructionPointer = ip + 1})

run :: ProgramState -> [Integer]
run = reverse . output . S.execState runProgram

opWithOperandToInts :: OpWithOperand -> [Integer]
opWithOperandToInts o = case o of
  Op Adv n -> [0,n]
  Op Bxl n -> [1,n]
  Op Bst n -> [2,n]
  Op Jnz n -> [3,n]
  Op Bxc n -> [4,n]
  Op Out n -> [5,n]
  Op Bdv n -> [6,n]
  Op Cdv n -> [7,n]


part1 :: Input -> [Integer]
part1 = run

part2 :: Input -> Maybe Integer
part2 ps@(ProgramState {program}) = recurseSolution (ps {registerA = 0}) (reverse . concatMap opWithOperandToInts $ A.elems program)

recurseSolution :: ProgramState -> [Integer] -> Maybe Integer
recurseSolution (ProgramState {registerA}) [] = Just registerA
recurseSolution ps@(ProgramState {registerA}) (x : xs) = do
  let vs =  filter (\i -> tryNumber i x ps) [0..7]
  let recurseVs = map (flip recurseSolution xs . (\i -> (ps {registerA = shift registerA 3 .|. i})))  vs
  case tracef ("registerA: " <> showIntAtBase 2 intToDigit  registerA "" <> " viable numbers: " <> show vs <> " program: " <> show (x:xs)) $ catMaybes recurseVs of
    [] -> Nothing
    (v:_) -> Just v


tryNumber :: Integer -> Integer -> ProgramState -> Bool
tryNumber i goal ps@(ProgramState {registerA}) =
  case run ps {registerA = shift registerA 3 + i} of
    [] -> False
    n | head n == goal ->  True
    _ -> False

prepare :: String -> Input
prepare inp = case runParser parser () "" inp of
  (Left e) -> error $ show e
  (Right res) -> res
  where
    parser :: Parser Input
    parser = do
      f <- ProgramState 0 <$> parseRegister 'A' <*> parseRegister 'B' <*> (parseRegister 'C' <* newline) <*> parseProgram
      return $ f []
    parseRegister :: Char -> Parser Integer
    parseRegister reg = read <$> (string "Register " *> char reg *> string ": " *> many1 digit) <* newline
    parseProgram :: Parser (A.Array Int OpWithOperand)
    parseProgram =
      string "Program: " *> do
        nums <- (map read <$> sepBy (many1 digit) (char ',')) :: Parser [Integer]
        let len = length nums
        let bounds = (0, (len `div` 2) - 1)
        return $ A.listArray bounds (popOpAndOperand nums)
    popOpAndOperand [] = []
    popOpAndOperand [_] = error "implied an odd number of numbers... shouldn't happen"
    popOpAndOperand (x : y : xs) = Op (convertToOp x) y : popOpAndOperand xs
    convertToOp 0 = Adv
    convertToOp 1 = Bxl
    convertToOp 2 = Bst
    convertToOp 3 = Jnz
    convertToOp 4 = Bxc
    convertToOp 5 = Out
    convertToOp 6 = Bdv
    convertToOp 7 = Cdv
    convertToOp _ = error "unknown op code"

main :: IO ()
main = readFile "inputs/day17input.txt" >>= print . (part1 &&& part2) . prepare