{-# LANGUAGE OverloadedStrings #-}

module D5_2019 where

import Prelude hiding (readFile)
import Data.Text (strip, unpack, splitOn)
import Data.Text.IO (readFile)

import Intcode

main :: IO ()
main = do
  input <- map (read . unpack) . splitOn "," . strip <$> readFile "D5/input.txt"

  case intcode (newVm { program = input, inputs = [1] }) of
    Left err -> putStrLn err
    Right st -> putStrLn ("Part 1: " ++ show (outputs st))

  case intcode (newVm { program = input, inputs = [5] }) of
    Left err -> putStrLn err
    Right st -> putStrLn ("Part 2: " ++ show (outputs st))
