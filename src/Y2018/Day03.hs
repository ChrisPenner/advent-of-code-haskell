module Y2018.Day03 where

import Data.Foldable (fold)
import Data.Char (isDigit)
import qualified Data.IntSet as IS
import qualified Data.Map as M

part1 :: IO ()
part1 = do
  idMap <- buildIDMap
  print . length . M.filter ((> 1) . IS.size) $ idMap

part2 :: IO ()
part2 = do
  idMap <- buildIDMap
  print $ findNonOverlappingID idMap

buildIDMap :: IO (M.Map (Int, Int) IS.IntSet)
buildIDMap = do
  input <- readFile "./input/2018-03.txt"
  let results = findFabricOverlay <$> lines input
  return $ M.unionsWith mappend results

findNonOverlappingID :: M.Map (Int, Int) IS.IntSet -> Int
findNonOverlappingID m = IS.findMin $ allInts IS.\\ overlappingInts
 where
  allInts         = fold m
  overlappingInts = fold . filter ((> 1) . IS.size) . M.elems $ m

findFabricOverlay :: String -> M.Map (Int, Int) IS.IntSet
findFabricOverlay s =
  let [idNum, sideMargin, topMargin, x, y] =
        fmap read . words $ map (\c -> if isDigit c then c else ' ') s
  in  M.fromList $ do
        x' <- [sideMargin + 1 .. sideMargin + x]
        y' <- [topMargin + 1 .. topMargin + y]
        return ((x', y'), IS.singleton idNum)
