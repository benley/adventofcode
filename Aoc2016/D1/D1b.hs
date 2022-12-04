-- http://adventofcode.com/2016/day/1
{-# LANGUAGE TupleSections #-}

module Main where

import Data.String.Utils (split, strip)

data Direction = North | East | South | West

runbunny :: Integral a => Read a =>
       [(a, a)]  -- coordinates already visited
    -> Direction -- current direction
    -> [String]  -- remaining commands
    -> [(a, a)]  -- final path, most recent node first
runbunny path _ [] = path

runbunny visited@((x,y):_) facing ((dir:distance):remaining_cmds) =
    runbunny (newpos:visited) newdir remaining_cmds where
      distN = read distance
      newpos = case newdir of North -> (x, y + distN); East  -> (x + distN, y);
                              South -> (x, y - distN); West  -> (x - distN, y)
      newdir | dir == 'R' = case facing of North -> East; East  -> South;
                                           South -> West; West  -> North
             | dir == 'L' = case facing of North -> West; West  -> South;
                                           South -> East; East  -> North
             | otherwise = error "wat"

findFirstDup :: Eq a => [a] -> a
findFirstDup (x:xs) = if x `elem` xs then x else findFirstDup xs

expand :: Integral a => [(a, a)] -> [(a, a)]
expand [c] = [c]
expand (c1 : c2 : cs) = expand_ c1 c2 ++ (tail . expand) (c2 : cs)
    where expand_ (x1, y1) (x2, y2)
            | x1 == x2 = map (x1,) (range y1 y2)
            | y1 == y2 = map (,y1) (range x1 x2)
            | otherwise = error "u wot m8"

          -- like [a..b] but it works when a < b
          range a b | a == b = [a] | otherwise = [a, (a + signum (b - a)) .. b]

main :: IO ()
main = do
    cmds <- map strip . split "," . strip <$> readFile "D1/input.txt"
    let seen = runbunny [(0, 0)] North cmds
    putStrLn ("Path: " ++ show seen)
    putStrLn ("Length: " ++ show (length seen))
    let (x, y) = head seen in
      putStrLn ("Part 1: final coordinate: " ++ show (x, y) ++
                ", distance from origin: " ++ show (abs x + abs y))
    let (x2, y2) = findFirstDup (expand . reverse $ seen) in
      putStrLn ("Part 2: first dup coordinate: " ++ show (x2, y2) ++
                ", distance from origin: " ++ show (abs x2 + abs y2))
