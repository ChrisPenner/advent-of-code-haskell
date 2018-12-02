module Y2018.Day02 where

import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Monoid
import Data.Bool
import Data.Foldable
import Control.Monad
import Data.List (tails)

part1 :: IO ()
part1 = do
  input <- readFile "./input/2018-02.txt"
  let counts = countLine <$> lines input
  print . uncurry (*) $ fold counts

countLine :: String -> (Sum Int, Sum Int)
countLine = (matchCount 2 &&& matchCount 3) . M.unionsWith mappend . fmap
  (`M.singleton` Sum (1 :: Int))
  where matchCount n = bool 0 1 . elem n

part2 :: IO ()
part2 = do
  inputLines <- lines <$> readFile "./input/2018-02.txt"
  print . head $ do
    (lineA, remainingLines) <- zip inputLines . drop 1 $ tails inputLines
    lineB                   <- remainingLines
    let notEqual = zipWith (/=) lineA lineB
    guard (1 == length (filter id notEqual))
    let paired     = zip notEqual lineA
    let equalChars = (snd <$> filter (not . fst) paired)
    return equalChars
