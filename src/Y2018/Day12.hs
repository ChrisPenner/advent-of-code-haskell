module Y2018.Day12 where

import Data.List.PointedList
import Control.Comonad
import Control.Lens

instance Comonad PointedList where
  extract = _focus
  duplicate = positions

parseInput :: IO ([Bool], [[Bool]])
parseInput = do
  (startLine : _ : rules) <- lines <$> readFile "./input/2018-12.txt"
  let startState    = fmap isAlive . (!! 2) . words $ startLine
  let filteredRules = filter (isAlive . last) rules
  let parsedRules   = fmap isAlive . head . words <$> filteredRules
  return (startState, parsedRules)
 where
  isAlive '#' = True
  isAlive _   = False


part1 :: IO ()
part1 = do
  (startState, rules) <- parseInput
  let Just pList = fromList startState
  print . score $ (iterate (step rules) pList !! 20)

score :: PointedList Bool -> Int
score l =
  sum
    .  fmap fst
    .  filter snd
    $  zip [-1, -2 ..] (l ^. reversedPrefix)
    ++ [(0, l ^. focus)]
    ++ zip [1 ..] (l ^. suffix)

step :: [[Bool]] -> PointedList Bool -> PointedList Bool
step rules = extend step' . pad
 where
  step' PointedList { _reversedPrefix = (l : ll : _), _focus = c, _suffix = (r : rr : _) }
    = [ll, l, c, r, rr] `elem` rules
  step' _ = False

pad :: PointedList Bool -> PointedList Bool
pad = (reversedPrefix <>~ [False, False]) . (suffix <>~ [False, False])

render :: [Bool] -> String
render = fmap (\b -> if b then '#' else '.')
