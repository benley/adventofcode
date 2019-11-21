-- good lord this is verbose

module D3_2018_p1 where

import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as MMerge
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Claim = Claim {
  index :: Int,
  leftOffset :: Int,
  topOffset :: Int,
  claimWidth :: Int,
  claimHeight :: Int
  }
  deriving Show

parseClaims :: Parser [Claim]
parseClaims = some parseClaim

parseClaim :: Parser Claim
parseClaim = do
  index_ <- char '#' >> L.decimal <* chunk " @ "
  leftOffset_ <- L.decimal <* char ','
  topOffset_ <- L.decimal <* chunk ": "
  width_ <- L.decimal <* char 'x'
  height_ <- L.decimal <* eol

  return $ Claim {
    index = index_,
    leftOffset = leftOffset_,
    topOffset = topOffset_,
    claimWidth = width_,
    claimHeight = height_
    }


type Grid = M.Map Position [Claim]

emptyGrid :: Grid
emptyGrid = M.fromList []

showGrid :: Grid -> String
showGrid grid = unlines [showLine y | y <- [0..yMax]]
  where
    xMax = maximum [x | (x, _) <- M.keys grid]
    yMax = maximum [y | (_, y) <- M.keys grid]
    showLine y = [showPx (x, y) | x <- [0..xMax]]
    showPx (x, y) =
      case length (M.lookup (x, y) grid) of
        0 -> '.'
        1 -> '1'
        _ -> 'X'

type Position = (Int, Int)

fill :: Claim -> Grid
fill c@Claim{ leftOffset = xOrigin
            , topOffset = yOrigin
            , claimWidth = width
            , claimHeight = height
            } =
  M.fromList [((x, y), [c]) | x <- [xOrigin..(xOrigin + width - 1)], y <- [yOrigin..(yOrigin + height - 1)]]

combineFills :: Grid -> Grid -> Grid
combineFills =
  MMerge.merge MMerge.preserveMissing
               MMerge.preserveMissing
               (MMerge.zipWithMatched (\_ a b -> a ++ b))

main :: IO ()
main = do
  input <- getContents
  let parsed = parse parseClaims "herp" input
  case parsed of
    Left err -> print err
    Right claims -> do

      -- Part 1
      let filledClaims = map fill claims
      let combined = foldl combineFills emptyGrid filledClaims
      let filtered = M.filter (\x -> length x >= 2) combined
      print $ M.size filtered

      -- Part 2
      let overlappingClaimIDs = Set.fromList $ concat $ M.elems $ M.map (map index) filtered
      let allClaimIDs = Set.fromList $ map index claims
      print $ Set.difference allClaimIDs overlappingClaimIDs
