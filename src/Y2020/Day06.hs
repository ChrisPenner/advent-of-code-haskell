{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
module Y2020.Day06 where

import Data.List  
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import Data.List
import Data.Function
import qualified Data.Map as M

main :: IO ()
main = do
    groups <- T.splitOn "\n\n" <$> T.readFile "./day06.txt"
    -- part 1
    print . sum $ length . S.fromList . T.unpack . T.filter (/= '\n')<$> groups
    -- part 2
    print . sum $ length . foldl1' S.intersection  . fmap (S.fromList . T.unpack) . T.lines <$> groups


sample :: IO ()
sample = do
  xs <- readFile "./scrap.txt"
  -- print . solve $ lines xs 
  print . fmap (fst . maximumBy (compare `on` snd) . M.toList . M.unionsWith (+) . fmap (flip M.singleton (1 :: Int))) . transpose . lines $ xs

solve ls = [maximumBy (compare `on` snd) [(c, length $ filter (== c) l) | c <- l] | l <- ls]
main' = do
  inp <- readFile "./scrap.txt"
  print (fst <$> solve (transpose (lines inp)))


-- solve :: [[Char]] -> (Char, Int)
-- solve input = maximumBy (compare `on` snd)  [(c,length $ filter (== c) l) | c <- ['a'..'z'] | l <- input]

