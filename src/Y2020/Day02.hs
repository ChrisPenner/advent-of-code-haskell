{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day02 where

import Data.List
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Lens
import Data.Text.Lens
import Control.Lens.Regex.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- main :: IO ()
-- main = do
--     rows <- fmap words . fmap (fmap toSpace) . lines <$> readFile "./day02.txt"
--     let Sum amt = flip foldMap rows $ \((read -> low):(read -> high):(letter:[]):password:[]) ->
--                     let actual = length $ filter (==letter) password
--                     in if actual >= low && actual <= high then Sum (1 :: Int)
--                                                else mempty
--     print amt
--     -- putStrLn $ unlines $ (fmap unwords) txt

-- toSpace :: Char -> Char
-- toSpace c | c `elem` ['a'..'z'] <> ['0'..'9'] = c
--   | otherwise = ' '


-- main2 :: IO ()
-- main2 = do
--     rows <- fmap words . fmap (fmap toSpace) . lines <$> readFile "./day02.txt"
--     let Sum amt = flip foldMap rows $ \((read -> low):(read -> high):(letter:[]):password:[]) ->
--                     let hasFirst = length $ filter (liftA2 (&&) ((== low) . fst) ((==letter) . snd)) $ zip [(1 :: Int) ..] password
--                         hasSecond = length $ filter (liftA2 (&&) ((== high) . fst) ((==letter) . snd)) $ zip [(1 :: Int) ..] password
--                      in if hasFirst + hasSecond == 1 then Sum (1 :: Int) else mempty
--     print amt



main' :: IO ()
main' = do
    rows <- T.lines <$> T.readFile "./day02.txt"
    print . length . filter checkRow $ rows
  where
    checkRow row =
        let [T.unpack -> read -> low, T.unpack -> read -> high, letter, password] = row ^. [regex|(\d+)-(\d+) (\w): (\w+)|] . groups
            amt = T.count letter password
        in amt >= low && amt <= high


main2' :: IO ()
main2' = do
    rows <- T.lines <$> T.readFile "./day02.txt"
    print . length . filter checkRow $ rows
  where
    checkRow row =
        let [T.unpack -> read -> pos1, T.unpack -> read -> pos2, T.unpack -> head -> letter, password] = row ^. [regex|(\d+)-(\d+) (\w): (\w+)|] . groups
         in lengthOf (reindexed succ text . indices (`elem` [pos1, pos2]) . only letter) password == 1
