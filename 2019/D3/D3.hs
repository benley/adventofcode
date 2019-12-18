{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module D3_2019 where

import Data.List.Split
import Diagrams.Prelude
import Diagrams.Backend.SVG

main :: IO ()
main = do
  [wire1, wire2] <- fmap (splitOn ",") . lines <$> readFile "D3/input.txt"
  runWithInput wire1 wire2

runWithInput :: [String] -> [String] -> IO ()
runWithInput wire1 wire2 = do
  let path1 = wireToPath wire1
      path2 = wireToPath wire2
      matchPoints = intersectPointsP path1 path2
  renderSVG "d3.svg" (dims (V2 1024 768)) $ mconcat [
    mconcat (map mkCircle matchPoints),
    strokePath path1 # lc blue,
    strokePath path2 # lc red,
    square 100 # lc green
    ]
  print $ minimum $ map manhattanDistance matchPoints
  where
    wireToPath = fromVertices . map p2 . cmdsToCoords . parseCommands
    mkCircle p = circle 1 # moveTo p

manhattanDistance :: Num a => P2 a -> a
manhattanDistance (P (V2 x y)) = abs x + abs y

data Command = WUp Float | WDown Float | WLeft Float | WRight Float deriving Show
type Wire = [Coord]
type Coord = (Float, Float)

parseCommands :: [String] -> [Command]
parseCommands (x:xs) = cmd : parseCommands xs
  where cmd = case x of
                'U':nn -> WUp    (read nn)
                'D':nn -> WDown  (read nn)
                'L':nn -> WLeft  (read nn)
                'R':nn -> WRight (read nn)
                c -> error ("bad command: " ++ show c)
parseCommands [] = []

cmdsToCoords :: [Command] -> [Coord]
cmdsToCoords = foldl applyCmd []

applyCmd :: [Coord] -> Command -> [Coord]
applyCmd path@((x, y) : _) cmd =
  case cmd of
    WUp    n -> (x, y+n) : path
    WDown  n -> (x, y-n) : path
    WLeft  n -> (x-n, y) : path
    WRight n -> (x+n, y) : path
applyCmd [] cmd = applyCmd [(0, 0)] cmd
