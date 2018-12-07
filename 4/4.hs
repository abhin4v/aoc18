{-# LANGUAGE Strict #-}
module Main where

import Control.Applicative (some)
import Data.Function (on)
import Data.List (sort, foldl', maximumBy, groupBy, sortOn)
import Data.List.Split (split, dropInitBlank, keepDelimsL, whenElt, chunksOf)
import qualified Data.Map as M
import Data.Time (LocalTime, diffUTCTime, localTimeToUTC, utc, todMin, localTimeOfDay)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Text.Parsec

type GuardId = Int
data Action = BeginShift GuardId
            | FallAsleep
            | WakeUp
            deriving (Show, Eq)
data Record = Record { recTime :: LocalTime, recAction :: Action }
              deriving (Show, Eq)
type Round = [Record]

shiftBeginning :: Record -> Bool
shiftBeginning (Record _ (BeginShift _)) = True
shiftBeginning _                         = False

instance Ord Record where
  compare = compare `on` recTime

parseTime :: String -> LocalTime
parseTime = parseTimeOrError False defaultTimeLocale "%F %R"

diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

inputP :: Parsec String () Record
inputP = Record <$> (tsP <* space) <*> (bsP <|> wP <|> aP)
  where
    intP = read <$> some digit
    tsP = parseTime <$> (char '[' *> some (noneOf "]") <* char ']')

    bsP =  BeginShift
          <$> (string "Guard" *> space *> char '#' *> intP <* space <* string "begins shift")

    wP = WakeUp <$ string "wakes up"
    aP = FallAsleep <$ string "falls asleep"

readInput :: [String] -> [Record]
readInput ls = case traverse (parse inputP "") ls of
  Left e -> error (show e)
  Right rs -> sort rs

splitRounds :: [Record] -> [[Record]]
splitRounds = split $ dropInitBlank $ keepDelimsL $ whenElt shiftBeginning

guardID :: Round -> GuardId
guardID ~(Record _ (BeginShift gID) : _) = gID

summarizeRounds :: [Round] -> (GuardId, Int, (Int, Int))
summarizeRounds rounds =
  (guardID $ head rounds,
   totalSleep rounds,
   case M.assocs minMap of
     [] -> (-1, 0)
     xs -> maximumBy (compare `on` snd) xs)
  where
    getMin = todMin . localTimeOfDay . recTime
    
    totalSleep = sum . map (sum . map (\[s,e] -> getMin e - getMin s) . chunksOf 2 . tail)
    minMap = 
      foldl' (\a r ->
              foldl' (\a' [s,e] ->
                      foldl' (\a'' m -> M.insertWith (+) m 1 a'') a' [getMin s .. (getMin e - 1)])
                     a (chunksOf 2 . tail $ r))
             M.empty rounds

mostAsleep = maximumBy (compare `on` (\(_,t,_) -> t))
mostAsleepOnSameMinute = maximumBy (compare `on` (\(_,_,(_,m)) -> m))

main = do
  rounds <- splitRounds . readInput . lines <$> getContents
  let summary = map summarizeRounds . groupBy ((==) `on` guardID) . sortOn guardID $ rounds
  putStrLn $ "Summary = " ++ show summary
  putStrLn $ "Most asleep = " ++ show (mostAsleep summary)
  putStrLn $ "Most asleep on same minute = " ++ show (mostAsleepOnSameMinute summary)
