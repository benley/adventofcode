module Main where

import Data.Char (isSpace)
import Data.IntMap qualified as M
import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec (runParser, sepEndBy, Parsec)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char (string, newline)

type Parser = Parsec Void String

-- Cmd <count> <from> <to>
data Cmd = Cmd Int Int Int deriving Show

parseCmd :: Parser Cmd
parseCmd = do
  c <- string "move " >> L.decimal
  f <- string " from " >> L.decimal
  t <- string " to " >> L.decimal
  return (Cmd c f t)

parseCmds :: Parser [Cmd]
parseCmds = parseCmd `sepEndBy` newline

digCol :: Int -> [String] -> String
digCol n xs = lstrip [x !! n | x <- xs]

lstrip :: String -> String
lstrip = dropWhile isSpace

type Stacks = M.IntMap String

runCmd :: Cmd -> Stacks -> Stacks
runCmd (Cmd 0 _ _) xs = xs
runCmd (Cmd 1 f t) xs =
  let x = head (xs M.! f) in M.adjust (x :) t $ M.adjust (drop 1) f xs
runCmd (Cmd c f t) xs = runCmd (Cmd (c-1) f t) (runCmd (Cmd 1 f t) xs)

runCmds :: [Cmd] -> Stacks -> Stacks
runCmds cmds stacks = foldl' (flip runCmd) stacks cmds

runCmdPart2 :: Cmd -> Stacks -> Stacks
runCmdPart2 (Cmd 0 _ _) xs = xs
runCmdPart2 (Cmd c f t) xs = do
  let x = take c (xs M.! f) in M.adjust (x ++) t $ M.adjust (drop c) f xs

runCmdsPart2 :: [Cmd] -> Stacks -> Stacks
runCmdsPart2 cmds stacks = foldl' (flip runCmdPart2) stacks cmds

main :: IO ()
main = do
  input <- lines <$> readFile "D5/input.txt"
  let rawStacks = lstrip <$> take 8 input
      stacks = M.fromList $ zip [1..] $ map (`digCol` rawStacks) (take (1 + length rawStacks) [1,5..])
      rawCmds = unlines $ drop 10 input
      cmds = case runParser parseCmds "asdf" rawCmds of
        Left err -> error (show err)
        Right asdf -> asdf

  putStr "Part 1: "
  putStrLn $ map (head . snd) $ M.toAscList (runCmds cmds stacks)

  putStr "Part 2: "
  putStrLn $ map (head . snd) $ M.toAscList (runCmdsPart2 cmds stacks)
