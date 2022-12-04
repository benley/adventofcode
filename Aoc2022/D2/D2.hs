-- | https://adventofcode.com/2022/day/2
-- | god this is ugly but whatever, it was quick.

module Main where

import Data.Void ( Void )
import Text.Megaparsec ( runParser, satisfy, sepEndBy, Parsec )
import Text.Megaparsec.Char ( newline, space )

type Parser = Parsec Void String

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

parseRps :: Parser RPS
parseRps = do
  c <- satisfy (`elem` ("ABCXYZ" :: String))
  return $ case c of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors
    _ -> error "impossible"

type Round = (RPS, RPS)

parseLine :: Parser Round
parseLine = do
  abc <- parseRps
  xyz <- space >> parseRps
  return (abc, xyz)

parseLines :: Parser [Round]
parseLines = parseLine `sepEndBy` newline

rpsValue :: RPS -> Int
rpsValue Rock = 1
rpsValue Paper = 2
rpsValue Scissors = 3

winValue :: RPS -> RPS -> Int
winValue Scissors Rock = 6
winValue Paper Scissors = 6
winValue Rock Paper = 6
winValue x y | x == y = 3
winValue _ _ = 0

scoreRound :: Round -> Int
scoreRound (abc, xyz) = rpsValue xyz + winValue abc xyz

main :: IO ()
main = do
  input <- readFile "D2/input.txt"
  case runParser parseLines "asdf" input of
    Left err -> print err
    Right rounds -> print $ sum (map scoreRound rounds)
