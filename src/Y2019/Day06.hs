{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Y2019.Day05 where

import Control.Lens
import Control.Lens.Regex.Text
import qualified Data.Text.IO as TIO
import Data.Map.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Function
import Control.Comonad.Traced
import Control.Comonad.Store
import Data.Graph
import Data.Maybe
import Data.Foldable

done :: M.Map T.Text T.Text -> Store T.Text (S.Set T.Text)
done orbits = extend wfix (store go "")
  where
    go :: T.Text -> Store T.Text (S.Set T.Text) -> S.Set T.Text
    go current solved = S.singleton current <>
        case M.lookup current orbits of
            Nothing -> mempty
            Just next -> peek next solved

doneT :: M.Map T.Text T.Text -> Traced T.Text (S.Set T.Text)
doneT orbits = extend wfix (traced go)
  where
    go :: T.Text -> Traced T.Text (S.Set T.Text) -> S.Set T.Text
    go current solved = S.singleton current <>
        case M.lookup current orbits of
            Nothing -> mempty
            Just next -> trace next solved

-- recipes :: Traced (S.Set String) (S.Set String)
-- recipes = traced (foldMap ingredientsOf)

-- allIngredientsFor :: Traced (S.Set String) (S.Set String)
-- allIngredientsFor = extend wfix (selectNext <$> listen recipes)
--   where
--     selectNext :: (S.Set String, S.Set String)
--                -> Traced (S.Set String) (S.Set String)
--                -> S.Set String
--     selectNext (requirements, input) t
--         | S.null (S.difference requirements input) = input
--         | otherwise = trace (S.difference requirements input) t


orbitFixPoint :: M.Map T.Text T.Text -> Traced T.Text (S.Set T.Text)
orbitFixPoint orbits = fix (traced . go)
  where
    go :: Traced T.Text (S.Set T.Text) -> T.Text ->  S.Set T.Text
    go solved current =
        case M.lookup current orbits of
            Nothing -> mempty
            Just next -> S.singleton next <> trace next solved

main :: IO ()
main = do
    orbits <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toMapOf (([regex|(\w+)\)(\w+)|] . groups . ito (\[a, b] -> (b, a))))
    let getNumDependents x = length $ trace x (orbitFixPoint orbits)
    let allPlanets = M.keys orbits
    print . sum $ (getNumDependents <$> allPlanets)

solveDependents :: M.Map T.Text T.Text -> T.Text -> (S.Set T.Text)
solveDependents directDeps = fix go
  where
    go :: (T.Text -> (S.Set T.Text)) -> (T.Text -> (S.Set T.Text))
    go solved next = (foldMapOf (ix next) S.singleton directDeps)
                     <> (foldMapOf (ix next) solved directDeps)

main2 :: IO ()
main2 = do
    orbits <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toMapOf (([regex|(\w+)\)(\w+)|] . groups . ito (\[a, b] -> (b, a))))
    let getNumDependents = length . solveDependents orbits
    let allPlanets = M.keys orbits
    print . sum $ (getNumDependents <$> allPlanets)

solveDependentsMap :: M.Map T.Text T.Text -> M.Map T.Text (S.Set T.Text)
solveDependentsMap directDeps = go
  where
    go :: M.Map T.Text (S.Set T.Text)
    go =  M.mapWithKey (\k v -> S.singleton k <> (go ^. ix v)) $ directDeps

main3 :: IO ()
main3 = do
    orbits <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toMapOf (([regex|(\w+)\)(\w+)|] . groups . ito (\[a, b] -> (b, a))))
    print . lengthOf (folded . folded) $ solveDependentsMap orbits


graphSolve :: IO ()
graphSolve = do
    (g, _) <- TIO.readFile "./src/Y2019/day06.txt"
                <&> toListOf (([regex|(\w+)\)(\w+)|] . groups . to (\[a, b] -> (b, b, [a]))))
                <&> graphFromEdges'
    print . sum $ (length . reachable g <$> vertices g)

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
