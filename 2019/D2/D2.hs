{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text as T (strip, pack, unpack, splitOn)

main :: IO ()
main = do
  input' <- splitOn "," . strip . pack <$> readFile "2019/D2/input.txt"
  let input = map (read . unpack) input'
      part1 = head $ intcode 0 (update 2 2 $ update 1 12 input)
      part2 = snd $ head $ filter (\(o, _) -> o==19690720) [tryNV n v input | n <- [0..99], v <- [0..99]]
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

-- | This will probably need to return useful errors at some point, but
-- | for now it can just return [-1] if it encounters unknown opcodes
intcode :: Int -> [Int] -> [Int]
intcode pos xs | xs!!pos == 99 = xs
intcode pos xs | opcode == 1 = intcode (pos+4) (update dest (xs!!a + xs!!b) xs)
               | opcode == 2 = intcode (pos+4) (update dest (xs!!a * xs!!b) xs)
               | otherwise = [-1]
  where (opcode:a:b:dest:_) = drop pos xs

-- | This is not efficient, but it's good enough for now
update :: Int -> a -> [a] -> [a]
update n newX xs = take n xs ++ [newX] ++ drop (n + 1) xs

-- | for part 2
tryNV :: Int -> Int -> [Int] -> (Int, Int)
tryNV n v xs = (head $ intcode 0 $ update 1 n $ update 2 v xs, 100 * n + v)
