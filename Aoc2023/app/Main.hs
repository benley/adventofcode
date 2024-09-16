-- |

module Main where

import D1.D1 (runD1)
import D2 (runD2)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["d1"] -> runD1
    ["d2"] -> runD2
    _ -> putStrLn "what?"
