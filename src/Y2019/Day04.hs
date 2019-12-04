{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Y2019.Day04 where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text.Lens
import Data.List
import qualified Data.Text as T

numbers :: [Int]
numbers = [307237..769058]

main :: IO ()
main = ([307237..769058] :: [Int])
        & lengthOf (traversed . re _Show
                   . filtered (\s -> s == sort s)
                   . filteredBy (packed . [regex|(\d)\1+|] . match . to T.length . only 2)
                   )
        & print

-- main :: IO ()
-- main = ([307237..769058] :: [Int])
--         & lengthOf (traversed . re _Show . filteredBy (packed . [regex|(\d)\1|] . match) . filtered (\s -> s == sort s))
--         & print

main2 :: IO ()
main2 = (307237 :: Int)
        & lengthOf
            ( takingWhile (<= 769058) (iterated succ)
            . re _Show
            . filteredBy (packed . [regex|(\d)\1+|] . match . to T.length . only 2)
            . filtered (\s -> s == sort s))
        & print



-- main :: IO ()
-- main = print . length $ do
--         n <- numbers
--         guard (sort (show n) == show n)
--         let doubles = (show n ^.. (packed . [regex|(\d)\1|] . match))
--         guard $ has (traversed . _head . to (\i -> length . filter (== i) $ show n) . only 2) doubles


