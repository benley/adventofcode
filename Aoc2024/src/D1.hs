module D1 where

import Data.List (sort, group)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.Text (lines, words, Text)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)
import Paths_Aoc2024 (getDataFileName)
import Prelude hiding (lines, readFile, words)

parse :: Text -> Int
parse n = case decimal n of
  Right (d, _) -> d
  _ -> error "wtf"

runD1 :: IO ()
runD1 = do
  input <- map words . lines <$> (readFile =<< getDataFileName "day01-input.txt")
  let col1 = sort $ map (parse . head) input
  let col2 = sort $ map (parse . last) input
  putStr "Part 1: "
  print $ sum $ zipWith (curry (abs . uncurry (-))) col1 col2
  putStr "Part 2: "
  let table = count col2
  print $ sum $ map (\n -> n * findWithDefault 0 n table) col1

-- I'm sure this is in a base library somewhere but I couldn't find it
count :: (Ord a) => [a] -> Map a Int
count = fromList . map (\x -> (head x, length x)) . group . sort
