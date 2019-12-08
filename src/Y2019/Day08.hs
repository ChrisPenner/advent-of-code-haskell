{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Y2019.Day08 where

import Control.Lens
import Data.Foldable
import Data.Ord
import Control.Applicative
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lens

groupUp :: Int -> [a] -> Maybe ([a], [a])
groupUp _ [] = Nothing
groupUp n xs = Just (splitAt n xs)

part1 :: IO ()
part1 = do
    readFile "./src/Y2019/day08.txt"
      <&> init
      <&> toListOf (unfolded (groupUp (25 * 6)))
      <&> minimumBy (comparing (lengthOf (traversed . only '0')))
      <&> liftA2 (*) (lengthOf (traversed . only '1')) (lengthOf (traversed . only '2'))
      >>= print

groupUp' :: Int -> [a] -> [[a]]
groupUp'  = unfoldr . groupUp

grouping :: Int -> Traversal [a] [b] [a] b
grouping n f s = s & (traverse f . groupUp' n)

transposing :: Iso [[a]] [[b]] [[a]] [[b]]
transposing = iso transpose transpose

-- part2 :: IO ()
-- part2 = do
--     readFile "./src/Y2019/day08.txt" <&> init
--       <&> transposeOf (grouping (25 * 6))
--       <&> traversed %~ (firstOf (traversed . filtered (/= '2')))
--       <&> view (below _Just)
--       <&> traversed . filteredBy (only '0') .~ ' '
--       <&> traversed . filteredBy (only '1') .~ '#'
--       <&> groupUp' 25
--       <&> unlines
--       >>= putStrLn

part2 :: IO ()
part2 = do
    T.readFile "./src/Y2019/day08.txt"
      <&> T.strip
      <&> T.chunksOf (25 * 6)
      <&> T.transpose
      <&> traversed %~ (firstOf (each . filtered (/= '2')))
      <&> view (below _Just . packed)
      <&> T.replace "0" " "
      <&> T.replace "1" "#"
      <&> T.chunksOf 25
      <&> T.unlines
      >>= T.putStrLn
