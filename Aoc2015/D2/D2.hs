module Main where

import Data.List.Split
import Text.Printf

doBox :: [Int] -> Int
doBox [x, y, z] = let s1 = x * y
                      s2 = x * z
                      s3 = y * z
                  in minimum [s1, s2, s3] + (2 * (s1 + s2 + s3))
doBox _ = 0

part1 :: [String] -> Int
part1 x = sum $ map (doBox . map strToInt . splitOn "x") x

part2 :: [String] -> Int
part2 x = sum $ map (ribbon . map strToInt . splitOn "x") x

ribbon :: [Int] -> Int
ribbon [x, y, z] = let p1 = 2 * (x + y)
                       p2 = 2 * (x + z)
                       p3 = 2 * (y + z)
                   in (x * y * z) + minimum [p1, p2, p3]
ribbon _ = 0

strToInt :: String -> Int
strToInt x = read x :: Int


main :: IO ()
main = do
    input <- readFile "D2/input.txt"
    printf "Part 1: %d\n" $ part1 (splitOn "\n" input)
    printf "Part 2: %d\n" $ part2 (splitOn "\n" input)
