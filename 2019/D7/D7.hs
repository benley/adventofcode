{-# LANGUAGE OverloadedStrings #-}

module D7_2019 where

import Control.Monad.Trans.State
import Data.List
import Data.Text (strip, unpack, splitOn)
import Data.Text.IO (readFile)
import Intcode
import Prelude hiding (lines, readFile)

type Phase = Int
type Signal = Int

runStep :: Program -> Phase -> Signal -> Signal
runStep p phase signal =
  case intcode (newVm { program = p, inputs = [phase,signal] }) of
    Left err -> error err
    Right VmState {outputs = [signal']} -> signal'
    _ -> error "wtf"

runAll :: Program -> [Phase] -> Signal -> Signal
runAll _ [] signal = signal
runAll p (phase:ps) signal =
  runAll p ps (runStep p phase signal)

main :: IO ()
main = do
  initTape <- map (read . unpack) . splitOn "," . strip <$> readFile "D7/input.txt"
  let phaseSettings = permutations [0..4]

  putStrLn $ "Part 1: " ++ show (maximum [runAll initTape xs 0 | xs <- phaseSettings])
  putStrLn $ "Part 2: " ++ show (runp2 initTape)

-- runp2 :: Program -> [Int]
runp2 p = do
  let vm = newVm{program=p}
  let ampAout = evalState (runIntcode (5:0:ampEout)) vm
      ampBout = evalState (runIntcode (8:ampAout)) vm
      ampCout = evalState (runIntcode (7:ampBout)) vm
      ampDout = evalState (runIntcode (6:ampCout)) vm
      ampEout = evalState (runIntcode (9:ampDout)) vm

  -- (ampAout, ampBout, ampCout, ampDout, ampEout)
  head ampEout
