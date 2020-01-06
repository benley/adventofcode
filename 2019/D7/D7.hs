{-# LANGUAGE OverloadedStrings #-}

module D7_2019 where

import Data.List
import Intcode

type Phase = Int
type Signal = Int

runAll :: Program -> [Phase] -> Signal -> Signal
runAll _ [] signal = signal
runAll p (phase:ps) signal = runAll p ps runStep
  where runStep = last (intcode [phase, signal] 0 p)

main :: IO ()
main = do
  prog <- progFromFile "D7/input.txt"
  putStr "Part 1: "
  print $ maximum [runAll prog ps 0 | ps <- permutations [0..4]]
  putStr "Part 2: "
  print $ maximum [p2Step prog ps | ps <- permutations [5..9]]

p2Step :: Program -> [Phase] -> Int
p2Step p [p1, p2, p3, p4, p5] = do
  let ampA = intcode (p1:0:ampE) 0 p
      ampB = intcode (p2:ampA) 0 p
      ampC = intcode (p3:ampB) 0 p
      ampD = intcode (p4:ampC) 0 p
      ampE = intcode (p5:ampD) 0 p
  last ampE
p2Step _ _ = error "wat"
