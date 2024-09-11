{-# LANGUAGE OverloadedStrings #-}

module D1.D1 where

import Data.Char (isDigit)
import Data.Text (filter, head, last, lines, Text, isPrefixOf, drop, take)
import Data.Text.IO (readFile)
import Paths_Aoc2023 (getDataFileName)
import Prelude hiding (filter, head, last, lines, readFile, drop, take)

filterDigits :: Text -> Int
filterDigits t =
  read [head ds, last ds]
  where ds = filter isDigit t

runD1 :: IO ()
runD1 = do
  input <- lines <$> (readFile =<< getDataFileName "day01-input.txt")
  print $ part1 input
  print $ part2 input

part1, part2 :: [Text] -> Int
part1 = sum . map  filterDigits
part2 = sum . map (filterDigits . fixPrefix)

fixPrefix :: Text -> Text
fixPrefix "" = ""
fixPrefix x
  | isDigit (head x) = take 1 x <> xs
  | "one"   `isPrefixOf` x = "1" <> xs
  | "two"   `isPrefixOf` x = "2" <> xs
  | "three" `isPrefixOf` x = "3" <> xs
  | "four"  `isPrefixOf` x = "4" <> xs
  | "five"  `isPrefixOf` x = "5" <> xs
  | "six"   `isPrefixOf` x = "6" <> xs
  | "seven" `isPrefixOf` x = "7" <> xs
  | "eight" `isPrefixOf` x = "8" <> xs
  | "nine"  `isPrefixOf` x = "9" <> xs
  | otherwise = xs
  where xs = fixPrefix (drop 1 x)
