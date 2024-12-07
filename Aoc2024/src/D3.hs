-- This would have been way easier in python or perl or awk or any other
-- language where there aren't 20 different incompatible regex libraries to
-- choose from
module D3 where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Text.ICU
import Data.Text.ICU.Replace (replaceAll)
import Paths_Aoc2024 (getDataFileName)
import Prelude hiding (readFile)
import Data.Maybe (catMaybes)
import Data.Text.Read (decimal)
import Data.Either (rights)

matchRe :: Regex
matchRe = regex [] "mul\\(([0-9]+),([0-9]+)\\)"

dropRe :: Regex
dropRe = regex [DotAll] "don't\\(\\).*?(do\\(\\)|$)"

doPart1 :: Text -> Int
doPart1 input = sum $ map product params
  where
    matches = findAll matchRe input
    params = map (\m -> fst <$> rights (decimal <$> catMaybes [group 1 m, group 2 m])) matches

runD3 :: IO ()
runD3 = do
  input <- readFile =<< getDataFileName "day03-input.txt"
  putStr "Part 1: "
  print (doPart1 input)

  putStr "Part 2: "
  let fixed = replaceAll dropRe "" input
  print (doPart1 fixed)
