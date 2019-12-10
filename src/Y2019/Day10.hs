{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Y2019.Day10 where

import Control.Lens
import Data.Ratio
import Data.Ord
import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import Data.List

sortAngles :: (Int, Int) -> [(Int, Int)] -> (M.Map (Int, Int, Maybe Rational) [(Int, Int)])
sortAngles (x, y) points = M.fromListWith (<>) $ do
    (x', y') <- points
    guard ((x', y') /= (x, y))
    let diffX = x' - x
    let diffY = y' - y
    let m = if diffX == 0 then Nothing
                          else Just (abs (fromIntegral diffY % fromIntegral diffX))
    return ((signum diffX, signum diffY, m), [(x', y')])

part1 :: IO ()
part1 = do
    asteroidLocs <- readFile "./src/Y2019/day10.txt" <&> toListOf ((lined <.> traversed <. only '#') . asIndex . swapped)
    let measurements = do
        (x, y) <- asteroidLocs
        let asdf = sortAngles (x, y) asteroidLocs
        return ((x, y), (length asdf) , asdf)
    print $ maximumBy (comparing (view _2)) measurements

-- part1 :: IO ()
-- part1 = do
--     asteroidLocs <- readFile "./src/Y2019/day10.txt" <&> toListOf ((lined <.> traversed <. only '#') . asIndex . swapped)
--     (30, 34)
--     let measurements = do
--         (x, y) <- asteroidLocs
--         let asdf = do
--             (x', y') <- asteroidLocs
--             guard ((x', y') /= (x, y))
--             let diffX = x - x'
--             let diffY = y - y'
--             let m = if diffY == 0 then Nothing
--                                    else Just (diffX % diffY)
--             return (signum diffX, signum diffY, m)
--         return ((x, y), (length $ S.fromList asdf) , S.fromList asdf)
--     print $ maximumBy (comparing (view _2)) measurements
--     -- traverse_ print $ measurements

-- circleOrd :: (Int, Int, Maybe Rational) -> (Int, Int, Maybe Rational) -> Ordering
-- circleOrd (x, y, z) (x', y', z') =
--     case (signumComp (x, y) (x', y')) of
--         Eq ->
--         x -> x

wrap :: Float -> Float
wrap n | n < 0 = wrap $ n + (2 * pi)
wrap n = n

toAngleish :: (Int, Int, Maybe Rational) -> Float
toAngleish args = wrap . (subtract (pi + (pi / 2))) $ case args of
    (_, dy, Nothing) -> atan2 (fromIntegral dy) 0
    (dx, dy, Just (abs -> r)) -> atan2 (fromIntegral $ fromIntegral dy * numerator r)
                              (fromIntegral $ fromIntegral dx * denominator r)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

part2 :: IO ()
part2 = do
    asteroidLocs <- readFile "./src/Y2019/day10.txt" <&> toListOf ((lined <.> traversed <. only '#') . asIndex . swapped)
    -- let ref = (11, 13)
    let ref = (30, 34)
    let angleMap = M.mapKeys toAngleish (sortAngles ref asteroidLocs)
    let sortedAngleMap = sortOn (manhattanDistance ref) <$> angleMap
    let flattened = concat $ unfoldr go $ M.elems sortedAngleMap
    -- print (flattened !! 0)
    -- print (flattened !! 1)
    -- print (flattened !! 2)
    -- print (flattened !! 9)
    -- print (flattened !! 19)
    -- print (flattened !! 49)
    -- print (flattened !! 99)
    -- print (flattened !! 198)
    print (flattened !! 199)
    -- print (flattened !! 200)
    -- print (flattened !! 298)


go :: [[(Int, Int)]] -> Maybe ([(Int, Int)], [[(Int, Int)]])
go xs =
    case foldMapOf (traversed . _Cons) ((\(a, b) -> (pure a, pure b))) xs of
        (x, rest) | (null x && null rest) -> Nothing
        (x, rest) -> Just (x, rest)
