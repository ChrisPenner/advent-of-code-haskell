{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Y2020.Day02 where

import Data.Function ((&))

main :: IO ()
main = do
    rows <- lines <$> readFile "./day03.txt"
    -- Step one
    print $ countTreesForRoute rows (3,1)
    -- Step Two
    print . product $ countTreesForRoute rows <$> [(1,1),(3,1),(5,1),(7,1),(1,2)]

countTreesForRoute :: [String] -> (Int, Int) -> Int
countTreesForRoute rows (horMovement, vertMovement) =
    zip [(0 :: Int)..] rows
    & (filter \(i, row) -> i `mod` vertMovement == 0 
                           && (cycle row !! ((i `div` vertMovement) * horMovement)) == '#')
    & length


-- main' :: IO ()
-- main' = do
--     rows <- lines <$> readFile "./day03.txt"
--     print . product$ foldMap ((:[]). test rows) [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- test :: [String] -> (Int, Int) -> Int
-- test rows (x, y) =
--     let Sum n = flip foldMap (zip [(0 :: Int)..] rows) $ \case
--                     (i, row) | i `mod` y /= 0 -> 0
--                              | otherwise -> if ((cycle row) !! ((i `div` y) * x)) == '#' then Sum 1 else Sum 0
--      in n

