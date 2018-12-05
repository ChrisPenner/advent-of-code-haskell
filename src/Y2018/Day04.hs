{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
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

type TimeMap = M.Map Int (M.Map Int (Sum Int))
data GuardState = GuardState { _activeGuard :: Int
                             , _awake :: Bool
                             , _startedSleep :: LocalTime
                             , _timeMap :: TimeMap
                             }
makeLenses ''GuardState

startState :: GuardState
startState = GuardState
  { _activeGuard  = (-1)
  , _awake        = True
  , _startedSleep = undefined
  , _timeMap      = mempty
  }


type Parser = Parsec () String

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
  void $ many (noneOf "\n")
  return $ Log time' action'
 where
  wakesUp    = WokeUp <$ string "wakes up"
  fallAsleep = Asleep <$ string "falls asleep"
  newShift   = do
    void $ string "Guard #"
    guardID <- read <$> many digitChar
    return (NewShift guardID)

part1 :: IO ()
part1 = do
  timeMap' <- buildTimeMap
  let sleepiestGuard  = getSleepiestGuard timeMap'
  let sleepiestMinute = getSleepiestMinute <$> M.lookup sleepiestGuard timeMap'
  print ((sleepiestGuard *) <$> sleepiestMinute)

part2 :: IO ()
part2 = do
  timeMap' <- buildTimeMap
  let (guardID, sleepyMinute) = getMostConsistentlySleepy timeMap'
  print $ (guardID, sleepyMinute)
  print $ guardID * sleepyMinute

getMostConsistentlySleepy :: TimeMap -> (Int, Int)
getMostConsistentlySleepy m =
  let
    listOfGuardPatterns :: [(Int, M.Map Int (Sum Int))]
    listOfGuardPatterns = M.toList m
    (guardID, sleepTimes) =
      maximumBy (compare `on` maximum . snd) listOfGuardPatterns
    sleepiestMinute = fst . maximumBy (compare `on` snd) $ M.toList sleepTimes
  in
    (guardID, sleepiestMinute)

buildTimeMap :: IO TimeMap
buildTimeMap = do
  input <- readFile "./input/2018-04.txt"
  let parsedLines = traverse (parseMaybe parseLine) $ lines input
  logs <- maybe (fail "failed to parse") pure $ parsedLines
  let sortedLogs = sortOn time logs
  let timeMap'   = collectLogs sortedLogs
  return timeMap'

collectLogs :: [Log] -> TimeMap
collectLogs logs =
  _timeMap . flip execState startState . forM logs $ \(Log logTime action') ->
    do
      case action' of
        NewShift i -> activeGuard .= i
        WokeUp     -> do
          awake .= True
          startedSleeping <- use startedSleep
          logSleepingTime startedSleeping logTime
        Asleep -> awake .= False >> startedSleep .= logTime

logSleepingTime :: LocalTime -> LocalTime -> State GuardState ()
logSleepingTime (localTimeOfDay -> todMin -> minStart) (localTimeOfDay -> todMin -> minEnd)
  = forM_ [minStart .. (minEnd - 1)] $ \t -> do
    guardID <- use activeGuard
    timeMap . at guardID . non mempty . at t . non mempty <>= Sum 1

getSleepiestGuard :: TimeMap -> Int
getSleepiestGuard m =
  fst . maximumBy (compare `on` snd) . M.toList $ fmap fold m

getSleepiestMinute :: M.Map Int (Sum Int) -> Int
getSleepiestMinute m = fst . maximumBy (compare `on` snd) . M.toList $ m
