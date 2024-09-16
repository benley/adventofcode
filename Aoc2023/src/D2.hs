{-# LANGUAGE StrictData #-}
-- | https://adventofcode.com/2023/day/2

module D2 where

import qualified Data.Map.Strict as M
import Data.Text.IO (readFile)
import Data.Text (Text, pack)
import Data.Void
import Paths_Aoc2023 (getDataFileName)
import Prelude hiding (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runD2 :: IO ()
runD2 = do
  input <- getDataFileName "day02-input.txt" >>= readFile
  let parseResult = runParser (many parseGame) "day02-input.txt" input
  case parseResult of
    Left err -> print err
    Right games -> do
      putStr "Part 1: "
      let validGames = filter validateGame games
      print (sum $ map gameId validGames)

      putStr "Part 2: "
      print (sum $ map minPower games)

data Game
   = Game
   { gameId :: Int
   , rounds :: [Round]
   } deriving (Show)

type Round = M.Map Text Int

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space hspace1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

parseGame :: Parser Game
parseGame = do
  gameId <- symbol "Game" *> lexeme L.decimal <* symbol ":"
  rounds <- many parseRound
  return $ Game { gameId = gameId, rounds = rounds }

parseRound :: Parser Round
parseRound = do
  colors <- parseColor `manyTill` (symbol ";" <|> eol)
  return $ M.fromList colors

parseColor :: Parser (Text, Int)
parseColor = do
  n <- lexeme L.decimal
  name <- lexeme (many letterChar) <* optional (symbol ",")
  return (pack name, n)

validateRound :: Round -> Bool
validateRound r =
  red <= 12 && green <= 13 && blue <= 14
  where red   = M.findWithDefault 0 "red"   r
        green = M.findWithDefault 0 "green" r
        blue  = M.findWithDefault 0 "blue"  r

validateGame :: Game -> Bool
validateGame g = all validateRound (rounds g)

minPower :: Game -> Int
minPower (Game {rounds}) = do
  let maxR = maximum (map (M.findWithDefault 0 "red")   rounds)
      maxG = maximum (map (M.findWithDefault 0 "green") rounds)
      maxB = maximum (map (M.findWithDefault 0 "blue")  rounds)
  maxR * maxG * maxB
