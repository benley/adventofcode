{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Intcode (
  intcode,
  toProgram,
  progFromFile,
  newVm,
  runIntcode,
  VmState(..),
  Program
) where

import qualified Data.IntMap as M
import Data.Text (strip, unpack, splitOn)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

type Address = Int

data Arg = Value Int
         | Ptr Address
         | Relative Address
         deriving Show

data Instruction = Add Arg Arg Arg
                 | Multiply Arg Arg Arg
                 | Store Arg
                 | Output Arg
                 | JumpIfTrue Arg Arg
                 | JumpIfFalse Arg Arg
                 | LessThan Arg Arg Arg
                 | Equal Arg Arg Arg
                 | SetRelBase Arg
                 | Halt
                 deriving Show

type Program = M.IntMap Int

-- | Convert an Int array containing an intcode program into a Program
toProgram :: [Int] -> Program
toProgram p = M.fromAscList (zip [0..] p)

-- | Read a file, return its contents as a Program
progFromFile :: FilePath -> IO Program
progFromFile f = toProgram . map (read . unpack) . splitOn "," . strip <$> readFile f

-- | Return the nth digit (from the right) of an integer, starting at 1
nthDigit :: Int -> Int -> Int
nthDigit nth num = num `div` (10 ^ (nth-1)) `mod` 10

-- | Decode the instruction at current position in program
decodeInstruction :: VmState -> Maybe Instruction
decodeInstruction VmState{program} | null program = Nothing
decodeInstruction VmState{program, position} =
  case opcode of
    1 -> Just $ Add         (argN 1) (argN 2) (argN 3)
    2 -> Just $ Multiply    (argN 1) (argN 2) (argN 3)
    3 -> Just $ Store       (argN 1)
    4 -> Just $ Output      (argN 1)
    5 -> Just $ JumpIfTrue  (argN 1) (argN 2)
    6 -> Just $ JumpIfFalse (argN 1) (argN 2)
    7 -> Just $ LessThan    (argN 1) (argN 2) (argN 3)
    8 -> Just $ Equal       (argN 1) (argN 2) (argN 3)
    9 -> Just $ SetRelBase  (argN 1)
    99 -> Just Halt
    _ -> Nothing
  where
    (argModes, opcode) = M.findWithDefault 0 position program `divMod` 100
    argN n = case nthDigit n argModes of
               0 -> Ptr v
               1 -> Value v
               2 -> Relative v
               x -> error ("Invalid instruction mode: " ++ show x)
             where v = M.findWithDefault 0 (position+n) program

data VmState = VmState { position :: Address
                       , relBase :: Address
                       , program :: Program }

newVm :: VmState
newVm = VmState { position = 0
                , relBase = 0
                , program = undefined }

-- | Run an intcode program with input
runIntcode :: Program -> [Int] -> [Int]
runIntcode prog inputs = intcode inputs (newVm { program = prog })

-- | Run an intcode VM
intcode :: [Int] -> VmState -> [Int]

intcode _ VmState{program} | null program = fail "Unexpected end of program"

intcode inputs vm@VmState{position=pos, program=xs, relBase} = do
  let memGet n = M.findWithDefault 0 n xs

      memPut (Value _) _ = error "Cannot write to an immediate-mode value"
      memPut (Ptr p) n = M.insert p n xs
      memPut (Relative r) n = M.insert (r + relBase) n xs

      getValue (Value n) = n
      getValue (Ptr p) = memGet p
      getValue (Relative r) = memGet (r + relBase)

  case decodeInstruction vm of

    Nothing -> error ("Could not decode opcode: " ++ show (memGet pos))

    Just (Add arg1 arg2 dest) ->
      intcode inputs vm { position = pos+4
                        , program = memPut dest (getValue arg1 + getValue arg2) }

    Just (Multiply arg1 arg2 dest) ->
      intcode inputs vm { position = pos+4
                        , program = memPut dest (getValue arg1 * getValue arg2) }

    Just (Store dest) ->
      intcode (drop 1 inputs) vm { position = pos+2
                                 , program = memPut dest (head inputs) }

    Just (Output arg) ->
      getValue arg : intcode inputs vm { position = pos+2 }

    Just (JumpIfTrue arg target) ->
      if getValue arg == 0
      then intcode inputs vm { position = pos+3 }
      else intcode inputs vm { position = getValue target }

    Just (JumpIfFalse arg target) ->
      if getValue arg == 0
      then intcode inputs vm { position = getValue target }
      else intcode inputs vm { position = pos+3 }

    Just (LessThan arg1 arg2 dest) -> do
      let v = if getValue arg1 < getValue arg2 then 1 else 0
      intcode inputs vm { position = pos+4, program = memPut dest v }

    Just (Equal arg1 arg2 dest) -> do
      let v = if getValue arg1 == getValue arg2 then 1 else 0
      intcode inputs vm { position = pos+4, program = memPut dest v }

    Just (SetRelBase arg) ->
      intcode inputs vm { position = pos+2, relBase = relBase + getValue arg }

    Just Halt -> []
