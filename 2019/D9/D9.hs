{-# LANGUAGE OverloadedStrings #-}

module Main where

import Intcode

main :: IO ()
main = do
  run <- runIntcode <$> progFromFile "2019/D9/input.txt"
  putStr "Part 1: "
  print $ run [1]
  putStr "Part 2: "
  print $ run [2]
