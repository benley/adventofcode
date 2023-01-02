module Aoc2022.D7.Main where

import Data.List (foldl', sort)
import Data.Functor
import Data.Graph.Inductive.Graph -- (mkGraph, prettyPrint)
import Data.Graph.Inductive.PatriciaTree -- (Gr)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer qualified as L

type Tree = Gr DirectoryEntry ()

data DirectoryEntry = FileE String Int | DirE String deriving (Show, Eq)

isDir :: Maybe DirectoryEntry -> Bool
isDir (Just (DirE _)) = True
isDir _               = False

initialT :: Tree
initialT = mkGraph [(1, DirE "/")] []

addEntryToTree :: DirectoryEntry -> Node -> Tree -> Tree
addEntryToTree de parentNode t = do
  let newNode = head $ newNodes 1 t
  insEdge (parentNode, newNode, ()) $ insNode (newNode, de) t

addEntries :: [DirectoryEntry] -> Node -> Tree -> Tree
addEntries ds cwd t = do
  -- TODO: ignore duplicates?
  let ns = newNodes (length ds) t
      es = [(cwd, n, ()) | n <- ns]
  (insEdges es . insNodes (zip ns ds)) t

mkdir :: String -> Node -> Tree -> Tree
mkdir d = addEntryToTree (DirE d)

addFile :: String -> Int -> Node -> Tree -> Tree
addFile f s = addEntryToTree (FileE f s)

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
parseChdir = string "$ cd " >> manyTill C.asciiChar newline <&> Chdir

parseListDir :: Parser Command
parseListDir = string "$ ls" >> newline >> parseLsOutput <&> ListDir

parseLsOutput :: Parser [DirectoryEntry]
parseLsOutput = many (parseLsDir <|> parseLsFile)

parseLsDir :: Parser DirectoryEntry
parseLsDir = do
  name <- string "dir " >> manyTill C.asciiChar newline
  return (DirE name)

parseLsFile :: Parser DirectoryEntry
parseLsFile = do
  fileSize <- L.decimal <* space
  name <- manyTill asciiChar newline
  return (FileE name fileSize)


main :: IO ()
main = do
  input <- readFile "D7/input.txt"
  case runParser parseCommands "asdf" input of
    Left err -> print err
    Right cmds -> do
      let (_, t) = foldl' runCmd (1, initialT) cmds
          dirNodes = [n | n <- nodes t, isDir (lab t n)]
          dirSizes = [sizeOfSubtree t n | n <- dirNodes]
      putStr "Part 1: "
      print (sum $ reverse $ sort (filter (< 100000) dirSizes))

      let diskSize = 70000000
          needed = 30000000
          used = sizeOfSubtree t 1
          free = diskSize - used
          needToFree = needed - free
      putStrLn ("Used: " ++ show used)
      putStrLn ("Free: " ++ show free)
      putStrLn ("Need to free: " ++ show needToFree)
      putStr "Part 2: "
      print (minimum (filter (>= needToFree) dirSizes))

type DState = (Node, Tree)

runCmd :: DState -> Command -> DState
runCmd (_, t) (Chdir "/") = (1, t)
runCmd st (Chdir ".") = st
runCmd (cwd, t) (Chdir "..") = (head (pre t cwd), t)

runCmd (cwd, t) (Chdir d) = do
  let x = [n | n <- suc t cwd, lab t n == Just (DirE d)]
  (head x, t)

runCmd (cwd, t) (ListDir ds) = (cwd, addEntries ds cwd t)
