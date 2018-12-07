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
import Control.Monad.Except
import Data.Graph.Inductive
import Data.Maybe


loadInput :: IO [String]
loadInput = lines . T.unpack . T.strip <$> TIO.readFile "./input/2018-07.txt"

part1 :: IO ()
part1 = do
  gr <- parseGraph <$> loadInput
  print $ traverseGraph gr

parseGraph :: [String] -> Gr Char ()
parseGraph instructions = mkGraph nodes' edges'
 where
  allLabels        = S.fromList $ foldMap (\(a, b) -> [a, b]) edgeLabels
  nodes'           = (toNode &&& id) <$> (toList allLabels)
  edges' = (\(src, dest) -> (toNode src, toNode dest, ())) <$> edgeLabels
  edgeLabels       = parseInstruction <$> instructions
  parseInstruction = (head . (!! 1) &&& head . (!! 7)) . words

toNode :: Char -> Node
toNode = subtract 65 . ord
fromNode :: Node -> Char
fromNode = chr . (+ 65)

traverseGraph :: Gr Char () -> [Char]
traverseGraph gr =
  fmap fromNode
    . either id        id
    . flip   evalState (gr, mempty)
    . runExceptT
    $ traverseGraph'

traverseGraph' :: ExceptT [Node] (State (Gr Char (), [Node])) a
traverseGraph' = do
  gr <- use _1
  let availableNodes = nodes . nfilter ((== 0) . indeg gr) $ gr
  when (null availableNodes) (use _2 >>= throwError)
  let currentNode = minimum availableNodes
  _2 <>= [currentNode]
  _1 %= delNode currentNode
  traverseGraph'
