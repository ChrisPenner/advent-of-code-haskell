{-# LANGUAGE TemplateHaskell #-}
module Y2018.Day04 where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Monoid
import Data.Time
import Data.Foldable
import Data.Function
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (noneOf)
import Data.List as L
import qualified Data.Map as M

type Parser = Parsec () String
type GuardID = Int
type Minute = Int
type MinuteMap = M.Map Minute (Sum Int)
type TimeMap = M.Map GuardID MinuteMap
data GuardState = GuardState { _activeGuard :: GuardID
                             , _startedSleep :: Minute
                             , _timeMap :: TimeMap
                             }
makeLenses ''GuardState

startState :: GuardState
startState = GuardState
  { _activeGuard  = undefined
  , _startedSleep = undefined
  , _timeMap      = mempty
  }


data Action = Asleep | WokeUp | NewShift Int
  deriving Show

data Log = Log
  { time :: LocalTime
  , action :: Action
  } deriving Show

parseLine :: Parser Log
parseLine = do
  timeString <- between (char '[') (char ']') (some $ noneOf "]")
  space
  action' <- choice [newShift, wakesUp, fallAsleep]
  time'   <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" timeString
  return $ Log time' action'
 where
  wakesUp    = WokeUp <$ string "wakes up"
  fallAsleep = Asleep <$ string "falls asleep"
  newShift   = do
    void $ string "Guard #"
    guardID <- read <$> many digitChar
    return (NewShift guardID)

collectLogs :: [Log] -> TimeMap
collectLogs = _timeMap . flip execState startState . traverse processLog

processLog :: Log -> State GuardState ()
processLog (Log logTime action') = do
  let currentMin = todMin . localTimeOfDay $ logTime
  case action' of
    NewShift i -> activeGuard .= i
    WokeUp     -> do
      startedSleeping <- use startedSleep
      logSleepingTime startedSleeping currentMin
    Asleep -> startedSleep .= currentMin

logSleepingTime :: Minute -> Minute -> State GuardState ()
logSleepingTime minStart minEnd = forM_ [minStart .. (minEnd - 1)] $ \t -> do
  guardID <- use activeGuard
  timeMap . at guardID . non mempty . at t . non mempty <>= Sum 1

buildTimeMap :: IO TimeMap
buildTimeMap = do
  input <- readFile "./input/2018-04.txt"
  let parsedLines =
        traverse (parseMaybe (parseLine <* many anyChar)) $ lines input
  logs <- maybe (fail "failed to parse") pure parsedLines
  let sortedLogs = sortOn time logs
  let timeMap'   = collectLogs sortedLogs
  return timeMap'

getMostConsistentlySleepy :: TimeMap -> (GuardID, Minute)
getMostConsistentlySleepy m =
  let (guardID, sleepTimes) =
        maximumBy (compare `on` maximum . snd) . M.toList $ m
  in  (guardID, getSleepiestMinute sleepTimes)

getSleepiestGuard :: TimeMap -> (GuardID, MinuteMap)
getSleepiestGuard = maximumBy (compare `on` fold . snd) . M.toList

getSleepiestMinute :: MinuteMap -> Minute
getSleepiestMinute = fst . maximumBy (compare `on` snd) . M.toList

part1 :: IO ()
part1 = do
  timeMap' <- buildTimeMap
  let (sleepiestGuard, minuteMap) = getSleepiestGuard timeMap'
  let sleepiestMinute             = getSleepiestMinute minuteMap
  print $ sleepiestGuard * sleepiestMinute

part2 :: IO ()
part2 = do
  timeMap' <- buildTimeMap
  let (guardID, sleepyMinute) = getMostConsistentlySleepy timeMap'
  print $ guardID * sleepyMinute
