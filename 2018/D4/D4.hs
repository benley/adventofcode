module D4_2018 where

import qualified Data.List
import qualified Data.Map as M
import Data.Time
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type GuardID = Int

type Minute = Int

type SleepRecord = M.Map GuardID [Minute]

data ShiftRecord =
    ShiftStart GuardID
  | FallAsleep UTCTime
  | WakeUp UTCTime
  deriving Show

type Parser = Parsec Void String

parseEntry :: Parser ShiftRecord
parseEntry = try parseShiftStart <|> try parseAsleep <|> parseWake

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  ts <- char '[' >> someTill (alphaNumChar <|> satisfy (`elem` "-: ")) (chunk "] ")
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" ts of
    Nothing -> fail "invalid timestamp"
    Just dt -> return dt

parseShiftStart :: Parser ShiftRecord
parseShiftStart = do
  guardId <- parseTimestamp >> chunk "Guard #" >> L.decimal
  _ <- chunk " begins shift"
  -- fail "FART"
  return (ShiftStart guardId)

parseAsleep :: Parser ShiftRecord
parseAsleep = do
  ts <- parseTimestamp
  _ <- chunk "falls asleep"
  return (FallAsleep ts)

parseWake :: Parser ShiftRecord
parseWake = do
  ts <- parseTimestamp <* chunk "wakes up"
  return (WakeUp ts)

-- data GuardShift = GuardShift {
--   guardId :: Int,
--   -- shiftStart :: LocalTime,
--   asleepMinutes :: [Int]
--   }

-- parseRecord :: Parser GuardShift
-- parseRecord = do
--   dt <- between (chunk "[") (chunk "]") parseDateTime
--   return $ GuardShift {
--     guardId = undefined,
--     asleepMinutes = undefined
--     }

-- parseDateTime :: Parser Data.Time.UTCTime
-- parseDateTime = undefined

main :: IO ()
main = do
  input <- Data.List.sort . lines <$> getContents
  -- case runParser parseRecord "herp" input of
  --   Left err -> print err
  --   Right records ->
  --     print records
  putStrLn $ unlines input
