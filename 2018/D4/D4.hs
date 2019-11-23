module D4_2018 where

import qualified Data.List as List
import qualified Data.Map as M
import Data.Time
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type GuardID = Int
type Minute = Int
type SleepRecord = M.Map GuardID [Minute]
data ShiftEvent = ShiftStart GuardID | FallAsleep | WakeUp deriving Show
type ShiftRecord = (UTCTime, ShiftEvent)
type Parser = Parsec Void String

parseRecords :: Parser [ShiftRecord]
parseRecords = parseRecord `sepEndBy` newline

parseRecord :: Parser ShiftRecord
parseRecord = do
  ts <- parseTimestamp
  event <- parseShiftStart <|> parseAsleep <|> parseWake
  return (ts, event)

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  ts <- char '[' >> someTill (alphaNumChar <|> satisfy (`elem` "-: ")) (chunk "] ")
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" ts of
    Nothing -> fail "invalid timestamp"
    Just dt -> return dt

parseShiftStart :: Parser ShiftEvent
parseShiftStart =
  ShiftStart <$> (chunk "Guard #" >> L.decimal <* chunk " begins shift")

parseAsleep :: Parser ShiftEvent
parseAsleep = chunk "falls asleep" >> return FallAsleep

parseWake :: Parser ShiftEvent
parseWake = chunk "wakes up" >> return WakeUp

main :: IO ()
main = do
  input <- getContents
  case runParser parseRecords "herp" input of
    Left err -> print err
    Right records ->
      print $ List.sortOn fst records
