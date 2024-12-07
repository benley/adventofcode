-- |

module Main where

import D1 (runD1)
import D2 (runD2)
import D3 (runD3)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["d1"] -> runD1
    ["d2"] -> runD2
    ["d3"] -> runD3
    _ -> putStrLn "what?"
