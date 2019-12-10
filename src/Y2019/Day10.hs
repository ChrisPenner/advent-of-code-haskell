{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Ord
import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import Linear hiding (transpose)
import Data.List

newtype Eps = Eps Double
  deriving newtype (RealFloat, Fractional, Real, RealFrac, Floating, Num, Epsilon)

instance Eq Eps where
  Eps a == Eps b = nearZero (a - b)

instance Ord Eps where
  compare a b | a == b = EQ
  compare (Eps a) (Eps b) = compare a b

getInput :: IO [V2 Int]
getInput = readFile "./src/Y2019/day10.txt"
  <&> toListOf ((lined <.> traversed <. only '#') . asIndex . to (uncurry (flip V2)))

part1 :: IO ()
part1 = do
    asteroidLocs <- getInput
    let measurements = do
        ref <- asteroidLocs
        let asdf = sortAngles ref asteroidLocs
        return (ref, (length asdf) , asdf)
    print . view _2 $ maximumBy (comparing (view _2)) measurements

-- Compute the angle of the vector after rotating by 90 and mirroring
-- (according to the problem), e.g. North is 0 and angles get larger clockwise
toAngle :: V2 Eps -> Eps
toAngle (V2 x y) = atan2 (negate x) y

manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance a b = sum . abs $ (a - b)

sortAngles :: V2 Int -> [V2 Int] -> (M.Map Eps [V2 Int])
sortAngles ref points = M.fromListWith (<>) $ do
    newpoint <- points
    guard (ref /= newpoint)
    return (toAngle . fmap fromIntegral $ (newpoint - ref), [newpoint])

part2 :: IO ()
part2 = do
    asteroidLocs <- getInput
    let ref = V2 30 34
    print $ asteroidLocs
          & sortAngles ref
          & M.elems
          & fmap (sortOn (manhattanDistance ref))
          & transpose
          & concat
          & (!! 199)
