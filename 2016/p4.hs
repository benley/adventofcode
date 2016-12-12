-- http://adventofcode.com/2016/day/4
--
-- I'm not thrilled with this code, but it does the thing.
module P4 where

import Data.Char (ord, chr)
import Data.List (group, sort, sortBy)

-- abcdef-gh-ijkl-432[abcde] -> ("abcdefghijkl", 432, "abcde")
parse :: String -> (String, Int, String)
parse xs = (encrypted_name, sector_id, checksum)
  where encrypted_name = filter (`elem` (['a'..'z'] ++ "-")) lhs
        sector_id = read (filter (`elem` ['0'..'9']) lhs)
        checksum = filter (`notElem` "[]") rhs
        (lhs, rhs) = span (/= '[') xs

isRealRoom :: (String, Int, String) -> Bool
isRealRoom (encname, sector_id, checksum) = topFiveLtrs == checksum
  where
    topFiveLtrs :: String
    topFiveLtrs = take 5 $ map snd (specialSort (lettercount encname))

    specialSort :: [(Int, Char)] -> [(Int, Char)]
    specialSort = sortBy (flip _sort_)

    _sort_ :: (Int, Char) -> (Int, Char) -> Ordering
    _sort_ (n1, c1) (n2, c2)
           | n1 < n2 = LT | n1 > n2 = GT
           | c1 < c2 = GT | c1 > c2 = LT

    lettercount :: String -> [(Int, Char)]
    lettercount = map classify . group . sort . filter (/= '-')

    classify xs = (length xs, head xs)

decrypt :: (String, Int, String) -> String
decrypt (encname, offset, _) = map (shift offset) encname
  where
    shift :: Int -> Char -> Char
    shift n '-' = ' '
    shift n c = int2char ((char2int c + n) `mod` 26)

    int2char :: Int -> Char
    int2char n = chr (ord 'a' + n)

    char2int :: Char -> Int
    char2int ch = ord ch - ord 'a'

main = do
    indata <- lines <$> readFile "p4-input.txt"
    let parsed_lines = map parse indata
        getId (a, b, c) = b

    putStr "Part 1: "
    print $ sum $ map getId $ filter isRealRoom parsed_lines

    putStrLn "Part 2:"
    let fucking (a, b) = show b ++ " " ++ a
    putStr $ unlines
           $ zipWith (curry fucking)
                     (map decrypt parsed_lines)
                     (map getId parsed_lines)
