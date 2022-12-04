module P2b where

import Data.List (foldl')
import Data.String.Utils (strip)

type Pos = (Int, Int)
type Keypad = (Pos -> Char)

padA :: Keypad
padA (x, y) =
    [ "     "
    , " 123 "
    , " 456 "  -- 5 is at (2, 2)
    , " 789 "
    , "     "
    ] !! y !! x

padB :: Keypad
padB (x, y) =
    [ "       "
    , "   1   "
    , "  234  "
    , " 56789 "  -- 5 is at (3, 1)
    , "  ABC  "
    , "   D   "
    , "       "
    ] !! y !! x

runpad :: Keypad -> Pos -> [String] -> String
runpad pad pos cmdlines = map pad (runpad' pad pos cmdlines) where

    runpad' _ _ [] = []
    runpad' pad pos (move_seq:moves) =
        newpos : runpad' pad newpos moves
        where newpos = foldl' (move pad) pos move_seq

    move :: Keypad -> Pos -> Char -> Pos
    move pinpad (x, y) dir =
        if pinpad newpos /= ' ' then newpos else (x, y)
        where newpos = case dir of
                         'U' -> (x, y - 1); 'L' -> (x - 1, y)
                         'D' -> (x, y + 1); 'R' -> (x + 1, y)

main = do
    cmds <- lines <$> readFile "p2-input.txt"
    putStrLn $ "Part 1: " ++ runpad padA (2,2) cmds
    putStrLn $ "Part 2: " ++ runpad padB (3,1) cmds
