module D4_2019 where

import Data.List

digits :: Int -> [Int]
digits = map (\c -> read [c]) . show

hasTwoAdjacent :: [Int] -> Bool
hasTwoAdjacent = any ((>1) . length) . group

hasExactlyTwoAdjacent :: [Int] -> Bool
hasExactlyTwoAdjacent = any ((==2) . length) . group

digitsIncrease :: [Int] -> Bool
digitsIncrease (n0:n1:ns) = n0 <= n1 && digitsIncrease (n1:ns)
digitsIncrease _ = True

check1 :: [Int] -> Bool
check1 n = hasTwoAdjacent n && digitsIncrease n

check2 :: [Int] -> Bool
check2 n = hasExactlyTwoAdjacent n && digitsIncrease n

main :: IO ()
main = do
  let digitsList = digits <$> [134792..675810]
  putStrLn $ "Part 1: " ++ show (length (filter check1 digitsList))
  putStrLn $ "Part 2: " ++ show (length (filter check2 digitsList))
