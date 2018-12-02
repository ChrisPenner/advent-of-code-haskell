-- https://adventofcode.com/2015/day/3
module Y2015.Day03 where

import Data.List as L
import Data.Set as S

run :: IO ()
run = do
  txt <- readFile "./input/2015-03.txt"
  let offsets                   = fmap toOffset txt
  let (roboOffset, humanOffset) = splitPositions offsets
  let scanPositions             = L.scanl' getNextPosition (0, 0)
  let humanPositions            = scanPositions humanOffset
  let roboPositions             = scanPositions roboOffset
  let allPositions              = S.fromList $ humanPositions <> roboPositions
  print $ length allPositions

getNextPosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
getNextPosition (prevX, prevY) (moveX, moveY) = (prevX + moveX, prevY + moveY)

toOffset :: Char -> (Int, Int)
toOffset '^' = (0, -1)
toOffset 'v' = (0, 1)
toOffset '>' = (1, 0)
toOffset '<' = (-1, 0)
toOffset _   = (0, 0)

splitPositions :: [a] -> ([a], [a])
splitPositions []  = ([], [])
splitPositions [a] = ([a], [])
splitPositions (a : b : rest) =
  let (aRest, bRest) = splitPositions rest in (a : aRest, b : bRest)
