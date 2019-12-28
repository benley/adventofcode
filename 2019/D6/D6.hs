{-# LANGUAGE OverloadedStrings #-}

module D6_2019 where

import Prelude hiding (lines, readFile)
import Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (lines, splitOn, Text)
import Data.Text.IO (readFile)
import Data.Set as Set
import Data.Graph.Inductive.Graph as G
-- import Data.Graph.Inductive.Query.GVD as GVD
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP as SP


-- | generate list of labeled nodes
-- genLNodes :: (Enum a) => a -> Int -> [LNode a]
-- genLNodes q i = take i (zip [1..] [q..])

-- | denote unlabeled edges
-- labUEdges :: [Edge] -> [UEdge]
-- labUEdges = Prelude.map (\(i,j) -> (i, j, ()))

main :: IO ()
main = do
  input <- fmap (splitOn ")") . lines <$> readFile "D6/input.txt"

  let
    allLabels :: [Text]
    allLabels = Set.toList $ Set.fromList $ concat input

    nodesByLabel = zip allLabels [1..]
    nodesById = zip [1..] allLabels

    nodeIDs :: M.Map Text Node
    nodeIDs = M.fromList nodesByLabel

    getNodeID = fromJust . flip M.lookup nodeIDs

    myEdges = Prelude.map (\[orbited, orbiting] -> ( getNodeID orbiting
                                                   , getNodeID orbited
                                                   , 1 )
                          ) input
    g :: Gr Text Int
    g = mkGraph nodesById myEdges

    rootNode = fromJust $ M.lookup "COM" nodeIDs

    -- v = gvdIn [rootNode] g
  -- prettyPrint g

  print $ "Order: " ++ show (G.order g)

  -- print $ G.suc g 123
  print $ "rootnode ID: " ++ show rootNode
  -- print v

  print $ SP.spLength 1058 376 g

-- dag3 :: Gr Char ()
-- dag3 = mkGraph (Prelude.zip [1..3] "abc") (labUEdges [(1,3)])
-- -- dag4 :: Gr Int ()
-- -- dag4 = mkGraph (genLNodes 1 4) (labUEdges [(1,2),(1,4),(2,3),(2,4),(4,3)])
-- asdf :: Gr String ()
-- asdf = mkGraph [(1, "bar")] [(1, 2, ())]
