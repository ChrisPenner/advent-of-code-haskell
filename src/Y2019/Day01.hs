{-# LANGUAGE TypeApplications #-}
module Day01 where

import Control.Lens
import Control.Lens.Action
import Data.Monoid

solve' :: IO (Sum Int)
solve' =  "./src/Y2019/day01.txt"
          ^! act readFile
          . worded
          . _Show @Double
          . to (/3)
          . to floor
          . to (subtract 2)
          . to Sum

main1' :: IO ()
main1' =  do
  input <- readFile "./src/Y2019/day01.txt"
  print $ input & sumOf ( worded
                . _Show
                . to (/ 3)
                . to (floor @Double @Int)
                . to (subtract 2))

solve :: IO ()
solve =  do
  input <- readFile "./src/Y2019/day01.txt"
  print $ input & sumOf ( worded
                        . _Show
                        . to (/ 3)
                        . to (floor @Double @Int)
                        . to (subtract 2)
                        )

main2' :: IO ()
main2' =  do
  input <- readFile "./src/Y2019/day01.txt"
  print
    $ input
    & sumOf (worded
             . _Show
             . (takingWhile (> 0) $ dropping 1 $ iterated (fromIntegral . subtract 2 . floor @Double @Int . (/ 3))))

solve2 :: IO ()
solve2 =  do
  input <- readFile "./src/Y2019/day01.txt"
  print
    $ input
    & sumOf ( worded 
            . _Show
            . (takingWhile (> 0) . dropping 1 . iterated) calculateRequiredFuel
            )
  where
    calculateRequiredFuel :: Double -> Double
    calculateRequiredFuel = (fromIntegral . subtract 2 . floor @Double @Int . (/ 3))


-- test :: Double
-- test = 100756 & sumOf (dropping 1 (takingWhile (>0) (iterated (fromIntegral . subtract 2 . floor @Double @Int . (/ 3)))))
