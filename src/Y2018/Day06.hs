module Y2018.Day06 where

import Data.Char
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Function
import Control.Applicative
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int)

loadInput :: IO [String]
loadInput = lines . T.unpack . T.strip <$> TIO.readFile "./input/2018-06.txt"

parsePoint :: String -> Point
parsePoint s = let [x, y] = fmap (read . filter isDigit) . words $ s in (x, y)

part1 :: IO ()
part1 = do
  points <- fmap parsePoint <$> loadInput
  let (Min minX, Min minY, Max maxX, Max maxY) =
        foldMap (\(x, y) -> (Min x, Min y, Max x, Max y)) points
  let allPoints      = liftA2 (,) [minX .. maxX] [minY .. maxY]
  let outerEdge      = buildOuterPoints (minX, minY) (maxX, maxY)
  let infinitePoints = findInfinitePoints points outerEdge
  let closenessMap   = getClosenessCounts points allPoints
  let filterInfinite p _ = p `S.notMember` infinitePoints
  print . getSum . maximum $ M.filterWithKey filterInfinite closenessMap


part2 :: IO ()
part2 = do
  points <- fmap parsePoint <$> loadInput
  let (Min minX, Min minY, Max maxX, Max maxY) =
        foldMap (\(x, y) -> (Min x, Min y, Max x, Max y)) points
  let pointsInRegion = liftA2 (,) [minX .. maxX] [minY .. maxY]
  print
    . length
    . filter id
    $ (withinRangeOfAll 10000 points <$> pointsInRegion)

withinRangeOfAll :: Int -> [Point] -> Point -> Bool
withinRangeOfAll dist points p =
  (< dist) . getSum $ foldMap (Sum . mDistance p) points

getClosenessCounts :: [Point] -> [Point] -> M.Map Point (Sum Int)
getClosenessCounts points allPoints =
  let closestPoints = getClosest points <$> allPoints
  in  M.unionsWith mappend . fmap (`M.singleton` Sum 1) $ closestPoints


buildOuterPoints :: Point -> Point -> [Point]
buildOuterPoints (minX, minY) (maxX, maxY) =
  [ (x, y) | x <- [minX, maxX], y <- [minY .. maxY] ]
    ++ [ (x, y) | y <- [minY, maxY], x <- [minX .. maxX] ]

findInfinitePoints :: [Point] -> [Point] -> S.Set Point
findInfinitePoints points outerEdge =
  S.fromList (getClosest points <$> outerEdge)

mDistance :: Point -> Point -> Int
mDistance (x, y) (px, py) = abs (x - px) + abs (y - py)

getClosest :: [Point] -> Point -> Point
getClosest points p = minimumBy (compare `on` mDistance p) points
