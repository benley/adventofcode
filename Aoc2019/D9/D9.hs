{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aoc2019.Intcode

main :: IO ()
main = do
  run <- runIntcode <$> progFromFile "Aoc2019/D9/input.txt"
  putStr "Part 1: "
  print $ run [1]
  putStr "Part 2: "
  print $ run [2]
