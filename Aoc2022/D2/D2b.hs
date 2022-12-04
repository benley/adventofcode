-- | https://adventofcode.com/2022/day/2
-- | weak-ass code.  whatever, it's done

module Main where

import Data.Void (Void)

import Text.Megaparsec ( runParser, satisfy, sepEndBy, Parsec )
import Text.Megaparsec.Char ( newline, space )

type Parser = Parsec Void String

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

data Ending = Win | Lose | Draw deriving (Show)

parseRps :: Parser RPS
parseRps = do
  c <- satisfy (`elem` ("ABC" :: String))
  return $ case c of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    _ -> error "wat"

parseEnding :: Parser Ending
parseEnding = do
  c <- satisfy (`elem` ['X','Y','Z'])
  return $ case c of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win
    _ -> error "wat"

type Round = (RPS, Ending)

parseLine :: Parser Round
parseLine = do
  theirMove <- parseRps
  goal <- space >> parseEnding
  return (theirMove, goal)

parseLines :: Parser [Round]
parseLines = parseLine `sepEndBy` newline

makeOutcome :: RPS -> Ending -> RPS
makeOutcome x Draw = x
makeOutcome Rock Win = Paper
makeOutcome Rock Lose = Scissors
makeOutcome Paper Win = Scissors
makeOutcome Paper Lose = Rock
makeOutcome Scissors Win = Rock
makeOutcome Scissors Lose = Paper

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
scoreRound (abc, xyz) = do
  let myMove = makeOutcome abc xyz
  rpsValue myMove + winValue abc myMove

main :: IO ()
main = do
  input <- readFile "D2/input.txt"
  case runParser parseLines "asdf" input of
    Left err -> print err
    Right rounds -> print $ sum (map scoreRound rounds)
