{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text (lines, unlines, strip, unpack, pack)
import           Data.Text.Format
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile, lines, unlines, print)
import           TextShow

type Coord = (Float, Float)

newtype Field = Field (S.Set Coord)

instance TextShow Field where
  showb f@(Field cs) =
    fromText $ unlines [showRow y | y <- [0..height]]
    where
      showRow y = pack [toC (x, y) | x <- [0..width]]
      toC coord  = if coord `elem` cs then '#' else '.'
      (width, height) = getBounds f

newtype CharField = CharField (M.Map Coord Char) deriving (Ord, Eq)

instance TextShow CharField where
  showb (CharField cs) =
    fromText $ unlines [showRow y | y <- [0..height]]
    where
      showRow y = pack [fromMaybe '.' (M.lookup (x, y) cs) | x <- [0..width]]
      width = maximum $ map fst $ M.keys cs
      height = maximum $ map snd $ M.keys cs

toCounts :: Field -> CharField
toCounts (Field cs) = CharField $
  M.fromList [(p, toC $ length $ findVisibleFromPosition (Field cs) p) | p <- S.toList cs]
  where
    toC x | x < 0 = '?'
          | x > 10 = '+'
          | otherwise = head (show x)

highlightVisibleFromPosition :: Field -> Coord -> CharField
highlightVisibleFromPosition field coord =
  CharField $ M.fromList $ (coord, '*') : map toC (findVisibleFromPosition field coord)
  where
    toC x = (x, '#')


getBounds :: Field -> Coord
getBounds (Field f) = (maximum xs, maximum ys) where
  xs = map fst (S.toList f)
  ys = map snd (S.toList f)

cansee :: Field -> Coord -> Coord -> Bool
cansee xs x y = null (findAllBetween xs x y)

findAllBetween :: Field -> Coord -> Coord -> [Coord]
findAllBetween (Field field) a b = [x | x <- S.toList field, x `isBetween` (a, b)]

isBetween :: Coord -> (Coord, Coord) -> Bool
b@(bx, by) `isBetween` (a@(ax, ay), c@(cx, cy)) =
  slope a b == slope a c
  && bx `inRange` (ax, cx)
  && by `inRange` (ay, cy)
  && b `notElem` [a, c]

slope :: Coord -> Coord -> Float
slope (x1, y1) (x2, y2) = (y1 - y2) / (x1 - x2)

inRange :: Ord a => a -> (a, a) -> Bool
inRange b (a, c) = (a >= b && b >= c)
                || (a <= b && b <= c)

findVisibleFromPosition :: Field -> Coord -> [Coord]
findVisibleFromPosition f@(Field field) pos =
  [x | x <- S.toList field, cansee f pos x, x /= pos]

loadFromFile :: FilePath -> IO Field
loadFromFile fp = do
  input <- map unpack . lines . strip <$> readFile fp

  let
    doLine :: Float -> String -> [Coord]
    doLine y zz = [(x, y) | (x, z) <- zip [0..] zz, z == '#']
    positions = concat [doLine y zz | (y, zz) <- zip [0..] input]
  return (Field (S.fromList positions))

main :: IO ()
main = do
  Field cs <- loadFromFile "D10/input.txt"

  let m2v = S.map (\coord -> (coord, length $ findVisibleFromPosition (Field cs) coord)) cs

  let ((bX, bY), num_visible) = last (sortOn snd (S.toList m2v))
  print "(part 1) Best location for monitoring station: ({}, {})\n" (shortest bX, shortest bY)
  print "         Visible from that location: {}\n" (Only num_visible)

debugFile :: FilePath -> IO ()
debugFile f = do
  Field cs <- loadFromFile f
  printT (toCounts (Field cs))

  mapM_ printT (S.map (highlightVisibleFromPosition (Field cs)) cs)
