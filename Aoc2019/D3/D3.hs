{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set as S
import Data.List (elemIndex)
import Data.List.Split
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "Aoc2019/D3/input.txt"
  runWithText input

runWithText :: String -> IO ()
runWithText input = do
  let [cmds1, cmds2] = map (splitOn ",") (lines input)
  runWithInput cmds1 cmds2

runWithInput :: [String] -> [String] -> IO ()
runWithInput cmds1 cmds2 = do
  let wire1 = fillCoords $ cmdsToCoords (parseCommands cmds1)
      wire2 = fillCoords $ cmdsToCoords (parseCommands cmds2)

      intersections = S.delete (0,0) (S.intersection (S.fromList wire1) (S.fromList wire2))
      stepsToEachIntersection = map (\c -> combinedStepsToPoint c wire1 wire2) (S.toList intersections)

  putStrLn $ "Part 1: " ++ show (minimum $ map manhattanDistance (S.toList intersections))
  putStrLn $ "Part 2: " ++ show (minimum stepsToEachIntersection)

combinedStepsToPoint :: Coord -> [Coord] -> [Coord] -> Int
combinedStepsToPoint pt wire1 wire2 = stepsToPoint pt wire1 + stepsToPoint pt wire2

stepsToPoint :: Coord -> [Coord] -> Int
stepsToPoint pt = fromJust . elemIndex pt

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y

data Command = WUp Int | WDown Int | WLeft Int | WRight Int deriving Show

type Coord = (Int, Int)

parseCommands :: [String] -> [Command]
parseCommands (x:xs) = cmd : parseCommands xs
  where cmd = case x of
                'U':nn -> WUp    (read nn)
                'D':nn -> WDown  (read nn)
                'L':nn -> WLeft  (read nn)
                'R':nn -> WRight (read nn)
                c -> error ("bad command: " ++ show c)
parseCommands [] = []

-- using reverse here is dumb but oh well
cmdsToCoords :: [Command] -> [Coord]
cmdsToCoords = reverse . foldl applyCmd []

applyCmd :: [Coord] -> Command -> [Coord]
applyCmd path@((x, y) : _) cmd =
  case cmd of
    WUp    n -> (x, y+n) : path
    WDown  n -> (x, y-n) : path
    WLeft  n -> (x-n, y) : path
    WRight n -> (x+n, y) : path
applyCmd [] cmd = applyCmd [(0, 0)] cmd

fillCoords :: [Coord] -> [Coord]
fillCoords = foldr applyFill []

applyFill :: Coord -> [Coord] -> [Coord]
applyFill (x1, y1) ((x0, y0) : xs) =
  [(x, y) | x <- fillBetween x1 x0, y <- fillBetween y1 y0] ++ xs
applyFill c [] = [c]

fillBetween :: (Num a, Ord a, Enum a) => a -> a -> [a]
fillBetween x0 x1 = case compare x0 x1 of
  LT -> [x0, x0+1 .. x1]
  GT -> [x0, x0-1 .. x1]
  EQ -> [x0]
