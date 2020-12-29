{-# LANGUAGE OverloadedStrings #-}

module D7_2019 where

import Data.List
import Aoc2019.Intcode

type Phase = Int
type Signal = Int

runAll :: Program -> [Phase] -> Signal -> Signal
runAll _ [] signal = signal
runAll p (phase:ps) signal = runAll p ps runStep
  where runStep = last (runIntcode p [phase, signal])

main :: IO ()
main = do
  prog <- progFromFile "Aoc2019/D7/input.txt"
  putStr "Part 1: "
  print $ maximum [runAll prog ps 0 | ps <- permutations [0..4]]
  putStr "Part 2: "
  print $ maximum [p2Step prog ps | ps <- permutations [5..9]]

p2Step :: Program -> [Phase] -> Int
p2Step p [p1, p2, p3, p4, p5] = do
  let run = runIntcode p
      ampA = run (p1:0:ampE)
      ampB = run (p2:ampA)
      ampC = run (p3:ampB)
      ampD = run (p4:ampC)
      ampE = run (p5:ampD)
  last ampE
p2Step _ _ = error "wat"
