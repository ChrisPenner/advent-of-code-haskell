{-# LANGUAGE ScopedTypeVariables #-}

module Y2020.Day09 where

import Control.Applicative
import Control.Monad.State
import Data.Traversable
import Data.List

main :: IO ()
main = do
    nums :: [Int] <- fmap (fmap read . lines) . readFile $ "./day09.txt"
    let preambleSize = 5
    let (preamble, rest) = splitAt preambleSize nums
    print . head . concat . fst . flip runState (reverse preamble) $ for rest $ \n -> do
        candidates <- gets (take preambleSize)
        let valid = any (\(a, b) -> a /= b && a + b == n) $ liftA2 (,) candidates candidates
        put (n : candidates)
        if valid then pure []
                 else pure [n]

main2 :: IO ()
main2 = do
    nums :: [Int] <- fmap (fmap read . lines) . readFile $ "./day09.txt"
    let target = 1309761972
    print . fmap findCode $ validSubsequences target nums
  where
    findCode xs = maximum xs + minimum xs

validSubsequences :: Int -> [Int] -> [[Int]]
validSubsequences target input =
    filter ((== target) . sum) . filter ((>2) . length) . fmap (takeTillSum target) $ prefixes
  where
    prefixes = tails input

takeTillSum :: Int -> [Int] -> [Int]
takeTillSum target input = go input 0
  where
    go [] _ = []
    go (x:rest) n =
        if x + n <= target
           then x : go rest (x + n)
           else []
