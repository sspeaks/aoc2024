{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day1 where
import Data.List (transpose, sort, group)


solve :: String -> Int
solve inp = let nums = map (map read . words) $ lines inp :: [[Int]]
                [a,b] = transpose nums
                abDiff :: Int -> Int -> Int
                abDiff n1 n2 = abs (n1 - n2)
             in sum $ zipWith abDiff (sort a) (sort b)

solve2 :: String -> Int
solve2 inp = let nums = map (map read . words) $ lines inp :: [[Int]]
                 [a,b] = transpose nums
                 lookupB = map (\a1 -> (head a, length a1)) $ group . sort $ b 
                 abDiff :: Int -> Int
                 abDiff n1 = case lookup n1 lookupB of
                    Nothing -> 0
                    Just cnt -> n1 * cnt
             in sum $ map abDiff a

main :: IO ()
main = do
    inp <- readFile "inputs/day1input.txt"
    print (solve inp, solve2 inp)
