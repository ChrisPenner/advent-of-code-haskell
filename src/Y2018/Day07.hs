{-# LANGUAGE TupleSections #-}
module Y2018.Day07 where

import Data.Char
import Data.List
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Function
import Control.Applicative
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Graph
import Control.Arrow ((&&&))
import Data.Array as A
import Control.Monad.State
import Control.Lens


loadInput :: IO [String]
loadInput = lines . T.unpack . T.strip <$> TIO.readFile "./input/2018-07.txt"

part1 :: IO ()
part1 = do
  (depGraph, vertToNode, charToVert) <- parseGraph <$> loadInput
  let startingPoints = getStartingPoints (indegree depGraph)
  let vertToChar     = (\(a, _, _) -> a) . vertToNode
  -- putStrLn . vertToChar $ traverseGraph depGraph startingPoints
  let (_, path') =
        flip execState (S.fromList startingPoints, [])
          $ traverseGraph' vertToNode charToVert
  print . fmap vertToChar $ path'
  -- print . fmap vertToChar $ topSort depGraph

traverseGraph'
  :: (Vertex -> (Char, Char, [Char]))
  -> (Char -> Maybe Vertex)
  -> State (S.Set Vertex, [Vertex]) ()
traverseGraph' lookupNode lookupVertex = do
  s <- use _1
  let mLinkedVerts = do
        (next, s') <- S.minView s
        let (_, _, linked) = lookupNode next
        linkedVerts <- traverse lookupVertex linked
        return (S.fromList linkedVerts <> s', next)
  case mLinkedVerts of
    Nothing              -> return ()
    Just (nextSet, used) -> do
      _1 .= nextSet
      _2 <>= [used]
      traverseGraph' lookupNode lookupVertex


getStartingPoints :: Array Vertex Int -> [Vertex]
getStartingPoints = fmap fst . filter ((== 0) . snd) . A.assocs

parseGraph
  :: [String] -> (Graph, Vertex -> (Char, Char, [Char]), Char -> Maybe Vertex)
parseGraph instructions = graphFromEdges edges'
 where
  edgeLabels  = sort (parseInstruction <$> instructions)
  startingMap = M.fromList ((, []) . snd <$> edgeLabels)
  dependencyMap :: M.Map Char [Char]
  dependencyMap =
    M.unionWith mappend startingMap
      $ M.fromListWith mappend
      . fmap (fmap pure)
      $ edgeLabels
  edges' = M.foldMapWithKey mkEdge dependencyMap
  mkEdge from tos = [(from, from, tos)]
  parseInstruction = (head . (!! 1) &&& head . (!! 7)) . words
