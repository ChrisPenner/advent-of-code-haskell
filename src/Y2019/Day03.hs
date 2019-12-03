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
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Category ((>>>))


testWires :: T.Text
testWires = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

data Dir = R | D | L | U
  deriving (Eq, Ord, Show, Read)

parseInput :: [String] -> (Dir, Int)
parseInput [d, n] = (read d, read n)
parseInput x = error $ "bad parseInput" <> show x

main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> each %~ ( toListOf ([regex|(\w)(\d+)|] . groups . mapping unpacked . to parseInput . folding flatten)
                      >>> scanl1 (\(a, b) (c, d) -> (a + c, b + d))
                      >>> S.fromList
                    )
               <&> foldl1 S.intersection
               <&> minimumOf (folded . to (sumOf (both . to abs)))
               >>= print
  where
    flatten :: (Dir, Int) -> [(Int, Int)]
    flatten (d, n) = replicate n (toCoord d)
    toCoord :: Dir -> (Int, Int)
    toCoord U = (0, -1)
    toCoord D = (0, 1)
    toCoord L = (-1, 0)
    toCoord R = (1, 0)

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

