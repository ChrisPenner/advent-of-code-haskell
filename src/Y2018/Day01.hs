module Y2018.Day01 where

import Data.List as L
import Data.Set as S
import Data.Maybe

readNumbers :: IO [Int]
readNumbers = do
  input <- readFile "./input/2018-01.txt"
  return $ read . dropWhile (== '+') <$> lines input

run1 :: IO ()
run1 = readNumbers >>= print . sum

firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated = snd . head . dropWhile (isNothing . snd) . L.scanl'
  go
  (S.empty, Nothing)
 where
  go :: Ord a => (Set a, Maybe a) -> a -> (Set a, Maybe a)
  go (s, Nothing) n =
    if S.member n s then (s, Just n) else (S.insert n s, Nothing)
  go (s, Just x) _ = (s, Just x)

run2 :: IO ()
run2 = do
  numbers <- readNumbers
  print $ firstRepeated (scanl' (+) 0 (cycle numbers))
