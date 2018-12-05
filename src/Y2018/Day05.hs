module Y2018.Day05 where

import Data.Char
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

loadPolymer :: IO String
loadPolymer = T.unpack . T.strip <$> TIO.readFile "./input/2018-05.txt"

part1 :: IO ()
part1 = do
  polymer <- loadPolymer
  print . length . reduce $ polymer

part2 :: IO ()
part2 = do
  polymer <- loadPolymer
  print . minimum . minusChars . reduce $ polymer

minusChars :: String -> [Int]
minusChars polymer = fmap without ['a' .. 'z']
  where without c = length . reduce $ filter ((/= c) . toLower) polymer

reduce :: String -> String
reduce = foldr' go ""
 where
  go c []       = [c]
  go c (x : xs) = if sameLetter && differentChar then xs else c : x : xs
   where
    sameLetter    = toLower x == toLower c
    differentChar = x /= c
