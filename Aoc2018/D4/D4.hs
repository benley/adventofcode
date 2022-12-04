module D4_2018 where

import Control.Arrow ((&&&))
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Time
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type GuardID = Int
type Minute = Int
data ShiftEvent = ShiftStart GuardID | FallAsleep | WakeUp deriving Show
type ShiftRecord = (UTCTime, ShiftEvent)
type Parser = Parsec Void String

type GuardLog = M.Map GuardID [Minute]

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

annotateRecords :: [ShiftRecord] -> GuardLog
annotateRecords ((_, ShiftStart firstGid) : xs') =
  continue firstGid M.empty xs' where
  continue :: GuardID -> GuardLog -> [ShiftRecord] -> GuardLog

  continue gid gl ((ts1, FallAsleep) : (ts2, WakeUp) : xs) =
    let updatedMap = M.insertWith (++) gid [minute ts1..(minute ts2 - 1)] gl in
    continue gid updatedMap xs

  continue _ gl ((_, ShiftStart nextGid) : xs) =
    continue nextGid gl xs

  continue _ gl [] = gl
  continue _ _ oops = error (show oops)

  minute :: UTCTime -> Minute
  minute ts = todMin $ localTimeOfDay $ utcToLocalTime utc ts

annotateRecords [] = M.empty
annotateRecords _ = error "wtf"

main :: IO ()
main = do
  input <- getContents
  case runParser parseRecords "herp" input of
    Left err -> print err
    Right r -> do
      let records = List.sortOn fst r
          gl = annotateRecords records
          mostAsleep :: (GuardID, [Minute])
          mostAsleep = M.foldrWithKey (\gid1 ms1 (gid2, ms2) ->
                                         if length ms1 > length ms2
                                         then (gid1, ms1)
                                         else (gid2, ms2)) (0, []) gl
          mostMinute :: Minute
          mostMinute =
            fst $ last $ List.sortOn snd $
            (map (head &&& length) . List.group . List.sort) (snd mostAsleep)

      print (fst mostAsleep * mostMinute)
