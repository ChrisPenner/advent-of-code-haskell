{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Y2019.Day06 where

import Control.Lens
import Control.Lens.Regex.Text
import qualified Data.Text.IO as TIO
import Data.Map.Lens
import qualified Data.Set as S
import Data.Function
import Data.Graph
import Data.Maybe
import Data.Foldable
import Control.Comonad
import Data.Tree
import Data.Tree.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Data.These
import Data.These.Lens
import Data.Monoid

graphSolve :: IO ()
graphSolve = do
    (g, _) <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (b, b, [a]))))
                <&> graphFromEdges'
    print . sum $ (length . reachable g <$> vertices g)


graphSolve' :: IO ()
graphSolve' = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] 
                             . groups 
                             . to (\[a, b] -> (b, b, [a])))
                             )
                <&> graphFromEdges'
                <&> lengthOf ( _1  -- Select the graph
                             -- stash the graph in the index for later
                             . (selfIndex
                               -- fold over vertices, keeping graph stashed
                               <. folding vertices
                               )
                                -- Pull the graph from the index alongside each vertex
                               . withIndex 
                               -- fold over every vertex reachable from every vertex!
                               . folding (uncurry reachable))
                >>= print

graphSolvePt2 :: IO ()
graphSolvePt2 = do
    (g, _, toVertex) <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (b, b, [a]))))
                <&> graphFromEdges
    let getReachable = S.fromList . reachable g . fromJust . toVertex
    let santa = getReachable "SAN"
    let you = getReachable "YOU"
    let diffs = (santa `S.union` you) S.\\ (santa `S.intersection` you)
    print $ (length diffs) - 2


forBlarg :: IO ()
forBlarg = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> (\x ->
                      fix (\r ->
                          toMapOf ( [regex|(\w+)\)(\w+)|]
                                  . groups . ito (\[a, b] -> (b, a))
                                  . to (\a -> r ^. at a . non (0 :: Int) . to succ)
                                  ) x
                          )
                    )
                <&> sum
                >>= print



treeSolve1 :: IO ()
treeSolve1 = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (a, [b]))))
                <&> M.fromListWith (<>)
                <&> (\m -> unfoldTree (buildTree m) "COM")
                <&> extend (sum . fmap length . children)
                <&> sum
                >>= print

buildTree :: M.Map T.Text [T.Text] -> (T.Text -> (T.Text, [T.Text]))
buildTree m k = (k, M.findWithDefault [] k m)

treeSolve2 :: IO ()
treeSolve2 = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (a, [b]))))
                <&> M.fromListWith (<>)
                <&> (\m -> unfoldTree (buildTree m) "COM")
                <&> foldTree connectToSanta
                <&> sumOf (traversed . both)
                >>= print

cataSet :: Setter (Tree a) b (a, [b]) b
cataSet = setting go
  where
    go :: ((a, [b]) -> b) -> (Tree a) -> b
    go f (Node a xs) = f (a, (go f <$> xs))

connectToSanta :: T.Text -> [Maybe (These (Sum Int) (Sum Int))] -> Maybe (These (Sum Int) (Sum Int))
connectToSanta "SAN" _ = Just $ This 0
connectToSanta "YOU" _ = Just $ That 0
connectToSanta _ xs = fold xs & _Just . (_This `failing` _That) +~ 1
-- connectToSanta _ xs = fold xs <&> \case
--   This x -> This $ x + 1
--   That x -> That $ x + 1
--   x -> x

extended :: (Comonad m, Traversable m) => Traversal (m a) (m b) (m a) b
extended f m = traverse f $ duplicate m

extended' :: (Comonad m, Foldable m) => Fold (m a) (m a)
extended' = folding duplicate


treeSolve1Blog :: IO ()
treeSolve1Blog = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (a, [b]))))
                <&> M.fromListWith (<>)
                <&> (\m -> unfoldTree (buildTree m) "COM")
                <&> lengthOf (extended' . branches . folded . folded)
                >>= print


treeSolve2Blog :: IO ()
treeSolve2Blog = do
    TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (a, [b]))))
                <&> M.fromListWith (<>)
                <&> (\m -> unfoldTree (buildTree m) "COM")
                <&> cataSet %~ uncurry connectToSanta
                <&> sumOf (traversed . both)
                >>= print

