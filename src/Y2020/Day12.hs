{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Y2020.Day06 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Graph
import Control.Lens.Regex.Text
import Control.Lens
import Data.Traversable
import Data.Monoid
import Data.Foldable
import Control.Monad.State

data Dir = N | E | S | W

main :: IO ()
main = do
    entries <- fmap lines . readFile $ "./day12.txt"
    let (Sum (x :: Int), Sum (y :: Int)) = fold . flip evalState E . for entries $ move
    print (abs x + abs y)
  where
    move :: String -> State Dir (Sum Int, Sum Int)
    move = \case
            ('N':(read -> n)) -> return (mempty, Sum n)
            ('S':(read -> n)) -> return (mempty, Sum (-n))
            ('E':(read -> n)) -> return (Sum n, mempty)
            ('W':(read -> n)) -> return (Sum (-n), mempty)
            ('L':(read -> n)) -> 
                modify (rotate False n) >> return mempty
            ('R':(read -> n)) ->
                modify (rotate True n) >> return mempty
            ('F':(read -> n)) ->
                get >>= \case
                  N -> return (mempty, Sum n)
                  S -> return (mempty, Sum (-n))
                  E -> return (Sum n, mempty)
                  W -> return (Sum (-n), mempty)
            s -> error s


-- main :: IO ()
-- main = do
--     entries <- fmap lines . readFile $ "./day12.txt"
--     let (Sum (x :: Int), Sum (y :: Int)) = fold . flip evalState E . for entries $ move
--     print (abs x + abs y)
--   where
--     move :: String -> State Dir (Sum Int, Sum Int)
--     move = \case
--             ('N':(read -> n)) -> return (mempty, Sum n)
--             ('S':(read -> n)) -> return (mempty, Sum (-n))
--             ('E':(read -> n)) -> return (Sum n, mempty)
--             ('W':(read -> n)) -> return (Sum (-n), mempty)
--             ('L':(read -> n)) -> 
--                 modify (rotate False n) >> return mempty
--             ('R':(read -> n)) ->
--                 modify (rotate True n) >> return mempty
--             ('F':(read -> n)) ->
--                 get >>= \case
--                   N -> return (mempty, Sum n)
--                   S -> return (mempty, Sum (-n))
--                   E -> return (Sum n, mempty)
--                   W -> return (Sum (-n), mempty)
--             s -> error s


-- rotate :: Bool ->  Int -> Dir -> Dir
-- rotate True 90 N = E
-- rotate _ 180 N = S
-- rotate True 270 N = W
-- rotate True 90 E = S
-- rotate _ 180 E = W
-- rotate True 270 E = N
-- rotate True 90 S = W
-- rotate _ 180 S = N
-- rotate True 270 S = E
-- rotate True 90 W = N
-- rotate _ 180 W = E
-- rotate True 270 W = S
-- rotate False 90 N = W
-- rotate False 270 N = E
-- rotate False 90 E = N
-- rotate False 270 E = S
-- rotate False 90 S = E
-- rotate False 270 S = W
-- rotate False 90 W = S
-- rotate False 270 W = N
