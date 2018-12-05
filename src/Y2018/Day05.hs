{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2018.Day05 where

import Data.Char
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


part1 :: IO ()
part1 = do
  polymer <- T.strip <$> TIO.readFile "./input/2018-05.txt"
  print . length . untilFixedPoint reduce . T.unpack $ polymer


part2 :: IO ()
part2 = do
  polymer <- T.strip <$> TIO.readFile "./input/2018-05.txt"
  print . minimum . minusChars $ T.unpack polymer

minusChars :: String -> [Int]
minusChars polymer = fmap without ['a' .. 'z']
 where
  without c =
    length . untilFixedPoint reduce $ filter ((/= c) . toLower) polymer

reduce :: String -> String
reduce = foldr' go ""
 where
  go c []       = [c]
  go c (x : xs) = if c /= x && toLower c == toLower x then xs else (c : x : xs)

untilFixedPoint :: Eq a => (a -> a) -> a -> a
untilFixedPoint f a = if f a == a then a else untilFixedPoint f (f a)
