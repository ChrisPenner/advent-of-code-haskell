{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Y2019.Day03 where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text.Lens
import Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Category ((>>>))
import Data.Map.Lens
import Linear
import Control.Monad


testWires :: T.Text
testWires = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

data Dir = R | D | L | U
  deriving (Eq, Ord, Show, Read)

parseInput :: [String] -> (Int, V2 Int)
parseInput [d, n] = (,) (read n) $ case d of
    "U" -> V2 0 (-1)
    "D" -> V2 0 1
    "L" -> V2 (-1) 0
    "R" -> V2 1 0


case_ :: Prism s t a b -> (a -> c) -> (t -> c) -> s -> c
case_ p f g = either g f . matching p

case__ :: Prism s t a b -> c -> (t -> c) -> s -> c
case__ p a = case_ p (const a)


otherwise_ :: a -> a
otherwise_ = id

otherwise__ :: a -> b -> a
otherwise__ = const


parseInput' :: (Char, String) -> (Int, V2 Int)
parseInput' (d, n) = (,) (read n) $ d &
    ( case__ (only 'U') (V2 0 (-1))
    $ case__ (only 'U') (V2 0 (-1))
    $ otherwise__ (V2 0 0))

    -- case_ (_DividedBy 3 . _DividedBy 5) (const " FizzBuzz")
  -- $ case_ (_DividedBy 3) (const "Fizz")
  -- $ case_ (_DividedBy 5) (const "Buzz")
  -- $ otherwise_ show

    -- (d & filtered (== 'U') .~ V2 0 (-1)
        -- )
    -- 'U' -> V2 0 (-1)
    -- 'D' -> V2 0 1
    -- 'L' -> V2 (-1) 0
    -- 'R' -> V2 1 0


main :: IO ()
main = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> both %~
                     (   toListOf ([regex|(\w)(\d+)|] . groups . mapping unpacked . to parseInput . folding (uncurry replicate))
                     >>> scanl1 (+)
                     >>> S.fromList
                     )
               <&> foldl1Of each S.intersection
               <&> minimumOf (folded . to (sum . abs))
               <&> print
               & join

main2 :: IO ()
main2 = do
    TIO.readFile "./src/Y2019/day03.txt"
               <&> T.lines
               <&> traverseOf both view (ix 0, ix 1)
               <&> each %~
                     (   toListOf ([regex|(\w)(\d+)|] . groups . mapping unpacked . to parseInput . folding (uncurry replicate))
                     >>> scanl1 (+)
                     >>> toMapOf (reindexed (+1) traversed . withIndex . swapped . ito id)
                     )
               <&> foldl1Of each (M.intersectionWith (+))
               <&> minimum
               >>= print

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
--     go ((x, y), locs) (L, n) =fixupfixup
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

