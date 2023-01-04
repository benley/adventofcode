module Main where

import Data.List (foldl', sort)
import Data.Functor
import Data.Graph.Inductive.Graph -- (mkGraph, prettyPrint)
import Data.Graph.Inductive.PatriciaTree
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (string, asciiChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Tree = Gr DirectoryEntry ()

data DirectoryEntry = FileE String Int | DirE String deriving (Show, Eq)

isDir :: Maybe DirectoryEntry -> Bool
isDir (Just (DirE _)) = True
isDir _               = False

addEntries :: [DirectoryEntry] -> Node -> Tree -> Tree
addEntries ds cwd t = do
  -- TODO: ignore duplicates? Doesn't matter for the AOC input data.
  let ns = newNodes (length ds) t
      es = [(cwd, n, ()) | n <- ns]
  (insEdges es . insNodes (zip ns ds)) t

sizeOfSubtree :: Tree -> Node -> Int
sizeOfSubtree t cwd =
  case lab t cwd of
    Just (FileE _ n) -> n
    Just (DirE _)    -> sum (map (sizeOfSubtree t) (suc t cwd))
    Nothing          -> error "wat"

type Parser = Parsec Void String

data Command = Chdir String | ListDir [DirectoryEntry] deriving Show

parseCommands :: Parser [Command]
parseCommands = many (parseChdir <|> parseListDir)

parseChdir :: Parser Command
parseChdir = string "$ cd " >> manyTill asciiChar newline <&> Chdir

parseListDir :: Parser Command
parseListDir = string "$ ls" >> newline >> parseLsOutput <&> ListDir

parseLsOutput :: Parser [DirectoryEntry]
parseLsOutput = many (parseLsDir <|> parseLsFile)

parseLsDir :: Parser DirectoryEntry
parseLsDir = string "dir " >> manyTill asciiChar newline <&> DirE

parseLsFile :: Parser DirectoryEntry
parseLsFile = do
  fileSize <- decimal <* space
  name <- manyTill asciiChar newline
  return (FileE name fileSize)

type DState = (Node, Tree)

runCmd :: DState -> Command -> DState
runCmd st       (Chdir ".")  = st
runCmd (  _, t) (Chdir "/")  = (1, t)
runCmd (cwd, t) (Chdir "..") = (head (pre t cwd), t)
runCmd (cwd, t) (Chdir    d) = (head [n | n <- suc t cwd, lab t n == Just (DirE d)], t)
runCmd (cwd, t) (ListDir ds) = (cwd, addEntries ds cwd t)

initialState :: DState
initialState = (1, mkGraph [(1, DirE "/")] [])

main :: IO ()
main = do
  input <- readFile "D7/input.txt"
  case runParser parseCommands "asdf" input of
    Left err -> print err
    Right cmds -> do
      let (_, t) = foldl' runCmd initialState cmds
          dirNodes = [n | n <- nodes t, isDir (lab t n)]
          dirSizes = [sizeOfSubtree t n | n <- dirNodes]
      putStr "Part 1: "
      print (sum $ reverse $ sort (filter (< 100000) dirSizes))

      let diskSize = 70000000; needed = 30000000; used = sizeOfSubtree t 1
          needToFree = needed - (diskSize - used)
      putStr "Part 2: "
      print (minimum (filter (>= needToFree) dirSizes))
