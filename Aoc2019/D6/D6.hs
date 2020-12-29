{-# LANGUAGE OverloadedStrings #-}

module D6_2019 where

import Prelude hiding (lines, readFile)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (lines, splitOn, Text)
import Data.Text.IO (readFile)
import qualified Data.Set as Set
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP (spLength)

main :: IO ()
main = do
  input <- fmap (splitOn ")") . lines <$> readFile "Aoc2019/D6/input.txt"

  let
    allLabels :: [Text]
    allLabels = Set.toList $ Set.fromList $ concat input

    nodesByLabel = zip allLabels [1..]
    nodesById = zip [1..] allLabels

    nodeIDs :: M.Map Text Node
    nodeIDs = M.fromList nodesByLabel

    getNodeID = fromJust . flip M.lookup nodeIDs

    myEdges = map (\[orbited, orbiting] -> (getNodeID orbiting, getNodeID orbited, 1))
                  input
    g :: Gr Text Int
    g = undir $ mkGraph nodesById myEdges

    -- for part 1
    rootNode = getNodeID "COM"
    allOrbits = sum $ catMaybes [spLength n rootNode g | n <- nodes g]

    -- for part 2
    (youNode, santaNode) = (getNodeID "YOU", getNodeID "SAN")
    orbitalTransfers = fromJust (spLength youNode santaNode g) - 2

  putStrLn $ "Part 1: " ++ show allOrbits
  putStrLn $ "Part 2: " ++ show orbitalTransfers
