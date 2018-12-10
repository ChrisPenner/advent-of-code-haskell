module Y2018.Day10 where

import Data.Char
import Data.List
import Data.Semigroup
import qualified Data.Set as S
import Control.Applicative

type Location = (Int, Int)
type Velocity = (Int, Int)
type Star = (Location, Velocity)

parseInput :: IO (S.Set Star)
parseInput = do
  lines' <- lines <$> readFile "./input/2018-10.txt"
  return . S.fromList $ fmap parseStar lines'

parseStar :: String -> Star
parseStar line =
  let [px, py, vx, vy] =
        fmap read . filter (not . null) . fmap (filter validChar) . words $ line
  in  ((px, py), (vx, vy))
  where validChar c = isDigit c || c == '-'

stepStar :: Star -> Star
stepStar ((px, py), (vx, vy)) = ((px + vx, py + vy), (vx, vy))

allSteps :: S.Set Star -> [S.Set Star]
allSteps = iterate (S.map stepStar)

isInRange :: S.Set Star -> Bool
isInRange s = let (_, minY, _, maxY) = getRange s in maxY - minY <= 9

getRange :: S.Set Star -> (Int, Int, Int, Int)
getRange s =
  let (Min minX, Min minY, Max maxX, Max maxY) =
        foldMap (\((px, py), _) -> (Min px, Min py, Max px, Max py)) s
  in  (minX, minY, maxX, maxY)

render :: S.Set Star -> String
render s = intercalate "\n" $ do
  y <- [0 .. maxY - minY]
  return $ liftA2 toChar [0 .. maxX - minX] [y]
 where
  toChar x y = if (x, y) `S.member` adjustedStarPositions then '#' else ' '
  (minX, minY, maxX, maxY) = getRange s
  adjustedStarPositions    = S.map (\((px, py), _) -> (px - minX, py - minY)) s

part1and2 :: IO ()
part1and2 = do
  stars <- parseInput
  let (before, message : _) = break isInRange . allSteps $ stars
  putStrLn . render $ message
  print $ length before
