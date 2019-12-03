{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Y2019.Day03 where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text.Lens
import Data.Text.IO as TIO
import Data.Map.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State
import qualified Data.Set as S
import Data.List


testWires :: T.Text
testWires = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

data Dir = R | D | L | U
  deriving (Eq, Ord, Show, Read)

parseInput :: [String] -> (Dir, Int)
parseInput [d, n] = (read d, read n)
parseInput x = error $ "bad parseInput" <> show x

-- main :: IO ()
-- main = do
--     input <- TIO.readFile "./src/Y2019/day03.txt"
--     let [wireOne, wireTwo] = T.lines input
--     let (pathOne, pathTwo) = (wireOne, wireTwo) & both %~ (toListOf ([regex|(\w)(\d+)|] . groups . mapping unpacked . to parseInput))
--     let (_, posOne) = getPositions pathOne
--     let (_, posTwo) = getPositions pathTwo
--     print $ minimumOf (folded . to (\(x, y) -> abs x + abs y)) (S.intersection posOne posTwo)
--     -- print input

-- getPositions :: [(Dir, Int)] -> ((Int, Int), S.Set (Int, Int))
-- getPositions = foldl' go ((0, 0), mempty)
--   where
--     go :: ((Int, Int), S.Set (Int, Int)) -> (Dir, Int) -> ((Int, Int), S.Set (Int, Int))
--     go ((x, y), locs) (U, n) =
--         let newLocs = S.fromList $ do offset <- [1..n]; return (x, y - offset)
--             in ((x,y - n), locs <> newLocs)
--     go ((x, y), locs) (D, n) =
--         let newLocs = S.fromList $ do offset <- [1..n]; return (x, y + offset)
--             in ((x,y + n), locs <> newLocs)
--     go ((x, y), locs) (L, n) =
--         let newLocs = S.fromList $ do offset <- [1..n]; return (x - offset, y)
--             in ((x - n, y), locs <> newLocs)
--     go ((x, y), locs) (R, n) =
--         let newLocs = S.fromList $ do offset <- [1..n]; return (x + offset, y)
--             in ((x + n, y), locs <> newLocs)


-- main2 :: IO ()
-- main2 = do
--     input <- TIO.readFile "./src/Y2019/day03.txt"
--     let [wireOne, wireTwo] = T.lines input
--     let (pathOne, pathTwo) = (wireOne, wireTwo) & both %~ (toListOf ([regex|(\w)(\d+)|] . groups . mapping unpacked . to parseInput))
--     let (_, _, posOne) = getPositions pathOne
--     let (_, _, posTwo) = getPositions pathTwo
--     print $ minimumOf folded (M.intersectionWith (+) posOne posTwo)
--     -- print input



-- getPositions2 :: [(Dir, Int)] -> (Int, (Int, Int), M.Map (Int, Int) Int)
-- getPositions2 = foldl' go (0, (0, 0), mempty)
--   where
--     go :: (Int, (Int, Int), M.Map (Int, Int) Int) -> (Dir, Int) -> (Int, (Int, Int), M.Map (Int, Int) Int)
--     go (totalSteps, (x, y), locs) (U, n) =
--         let newLocs = M.fromList $ do offset <- [1..n]; return ((x, y - offset), totalSteps + offset)
--             in (totalSteps + n, (x,y - n), locs <> newLocs)
--     go (totalSteps, (x, y), locs) (D, n) =
--         let newLocs = M.fromList $ do offset <- [1..n]; return ((x, y + offset), totalSteps + offset)
--             in (totalSteps + n, (x,y + n), locs <> newLocs)
--     go (totalSteps, (x, y), locs) (L, n) =
--         let newLocs = M.fromList $ do offset <- [1..n]; return ((x - offset, y), totalSteps + offset)
--             in (totalSteps + n, (x - n, y), locs <> newLocs)
--     go (totalSteps, (x, y), locs) (R, n) =
--         let newLocs = M.fromList $ do offset <- [1..n]; return ((x + offset, y), totalSteps + offset)
--             in (totalSteps + n, (x + n, y), locs <> newLocs)

