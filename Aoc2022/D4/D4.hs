-- | https://adventofcode.com/2022/day/4

module Main where

import Data.Void (Void)
import Text.Megaparsec ( runParser, sepBy1, sepEndBy, Parsec )
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer qualified as L

import Data.Text (pack, Text)
import Prelude hiding (lines)

type Parser = Parsec Void Text
type Range = (Int, Int)

parseRange :: Parser Range
parseRange = do
  [a, b] <- L.decimal `sepBy1` char '-'
  return (a, b)

parseLine :: Parser (Range, Range)
parseLine = do
  [a, b] <- parseRange `sepBy1` char ','
  return (a, b)

parseLines :: Parser [(Range, Range)]
parseLines = parseLine `sepEndBy` newline

contains :: Range -> Range -> Bool
contains (a1, b1) (a2, b2) = a1 <= a2 && b1 >= b2

contains' :: (Range, Range) -> Bool
contains' (a, b) = contains a b || contains b a

overlaps :: (Range, Range) -> Bool
overlaps ((a1, b1), (a2, b2)) = a1 <= a2 && b1 >= a2 || a2 <= a1 && b2 >= a1

main :: IO ()
main = do
  input <- pack <$> readFile "D4/input.txt"
  case runParser parseLines "asdf" input of
    Left err -> error (show err)
    Right parsed -> do
      putStr "Part 1: "
      print $ length $ filter contains' parsed

      putStr "Part 2: "
      print $ length $ filter overlaps parsed
