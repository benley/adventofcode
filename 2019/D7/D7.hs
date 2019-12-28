{-# LANGUAGE OverloadedStrings #-}

module D7_2019 where

import Prelude hiding (lines, readFile)
import Data.List
import Data.Text (strip, unpack, splitOn)
import Data.Text.IO (readFile)
import Intcode

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
