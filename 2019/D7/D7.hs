{-# LANGUAGE OverloadedStrings #-}

module D7_2019 where

import Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Data.List

import Intcode

type Phase = Int
type Signal = Int

runStep :: Program -> Phase -> Signal -> STM Signal
runStep p phase signal = do
  vm <- newVm p
  writeTBQueue (inputs vm) phase
  writeTBQueue (inputs vm) signal
  intcode vm
  readTBQueue (outputs vm)

runAll :: Program -> [Phase] -> Signal -> STM Signal
runAll _ [] signal = return signal
runAll p (phase:ps) signal = do
  stepResult <- runStep p phase signal
  runAll p ps stepResult

main :: IO ()
main = do
  initTape <- progFromFile "D7/input.txt"

  putStr "Part 1: "
  result1 <- maximum <$> atomically (mapM (\ps -> runAll initTape ps 0) (permutations [0..4]))
  print result1

  putStr "Part 2: "
  ret2 <- runp2 initTape
  print ret2
  -- putStrLn $ "Part 1: " ++ show (mapM_ maximum [runAll initTape xs 0 | xs <- phaseSettings])
  -- putStrLn $ "Part 2: " ++ show (runp2 initTape)


runp2 :: Program -> IO [Int]
runp2 p = do
  -- let vm = newVm{program=p}
  -- let ampAout = evalState (runIntcode (9:0:ampEout)) vm
  --     ampBout = evalState (runIntcode (8:ampAout)) vm
  --     ampCout = evalState (runIntcode (7:ampBout)) vm
  --     ampDout = evalState (runIntcode (6:ampCout)) vm
  --     ampEout = evalState (runIntcode (5:ampDout)) vm

  -- (ampAout, ampBout, ampCout, ampDout, ampEout)
  -- ampEout
  ampA_ <- atomically $ newVm p
  -- let outA = outputs ampA
  ampB_ <- atomically $ newVm p
  ampC_ <- atomically $ newVm p
  ampD_ <- atomically $ newVm p
  ampE_ <- atomically $ newVm p
  let ampA = ampA_ { inputs = outputs ampE }
      ampB = ampB_ { inputs = outputs ampA }
      ampC = ampC_ { inputs = outputs ampB }
      ampD = ampD_ { inputs = outputs ampC }
      ampE = ampE_ { inputs = outputs ampD }
  atomically (do
                 writeTBQueue (inputs ampA) 9
                 writeTBQueue (inputs ampA) 0
                 writeTBQueue (inputs ampB) 8
                 writeTBQueue (inputs ampC) 7
                 writeTBQueue (inputs ampD) 6
                 writeTBQueue (inputs ampE) 5
             )
  a <- async $ atomically $ intcode ampA
  b <- async $ atomically $ intcode ampB
  c <- async $ atomically $ intcode ampC
  d <- async $ atomically $ intcode ampD
  e <- async $ atomically $ intcode ampE
  wait a
  wait b
  wait c
  wait d
  wait e

  atomically $ flushTBQueue (outputs ampE)
