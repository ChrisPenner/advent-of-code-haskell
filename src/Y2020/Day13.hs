{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Y2020.Day13 where

import Control.Applicative
import Control.Monad.State
import Data.Traversable
import Data.List
import Data.Function

main :: IO ()
main = do
    [read -> (timestamp :: Int), buses] <- fmap lines . readFile $ "./day13.txt"
    let busIDs = splitter buses
    let closeness :: [(Int, Double)] = fmap (\x -> (x, let result = (fromIntegral timestamp / fromIntegral x) in result - fromIntegral (floor result :: Int))) busIDs
    print closeness
    let (closestBusID,_) = maximumBy (compare `on` snd) closeness
    let nextTime = ((timestamp `div` closestBusID) + 1) * closestBusID
    print $ (nextTime - timestamp) * closestBusID
    -- print busIDs
  where
    splitter :: String -> [Int]
    splitter = fmap read . filter (/= "x") . words . fmap \case
        ',' -> ' '
        x -> x

main' :: IO ()
main' = do
    [read -> (timestamp :: Int), buses] <- fmap lines . readFile $ "./day13.txt"
    let busPairs = sortOn snd $ splitter buses
    let (hOffset, hNum) = maximumBy (compare `on` snd) busPairs
    let result = head $ do
        startPoint <- fmap (subtract hOffset) . fmap (* hNum) $ [1..]
        guard . flip all busPairs $ \(offset, busID) -> ((startPoint + offset) `mod` busID) == 0
        return startPoint
    print result
        -- busIDs
  where
    splitter :: String -> [(Int, Int)]
    splitter = fmap (fmap read) . filter ((/= "x") . snd) . zip [0..] . words . fmap \case
        ',' -> ' '
        x -> x

multiplesToCheck :: [(Int, Int)] -> Int
multiplesToCheck xs@((_, x):_) = foldl' checker x $ xs
  where
    checker a (offset, n) = n - offset


