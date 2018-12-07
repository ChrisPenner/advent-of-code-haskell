{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Y2018.Day07 where

import Data.Char
import Data.List
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Lens
import Control.Monad.Except
import Data.Graph.Inductive


type Time = Int

data S =
  S { _graph :: Gr Char ()
    , _jobs :: M.Map Node Time
    , _timer :: Int
    }

makeLenses ''S

loadInput :: IO [String]
loadInput = lines . T.unpack . T.strip <$> TIO.readFile "./input/2018-07.txt"

part1 :: IO ()
part1 = do
  gr <- parseGraph <$> loadInput
  print $ traverseGraph gr

part2 :: IO ()
part2 = do
  gr <- parseGraph <$> loadInput
  print $ timedTraverseGraph gr

parseGraph :: [String] -> Gr Char ()
parseGraph instructions = mkGraph nodes' edges'
 where
  allLabels        = S.fromList $ foldMap (\(a, b) -> [a, b]) edgeLabels
  nodes'           = (toNode &&& id) <$> (toList allLabels)
  edges' = (\(src, dest) -> (toNode src, toNode dest, ())) <$> edgeLabels
  edgeLabels       = parseInstruction <$> instructions
  parseInstruction = (head . (!! 1) &&& head . (!! 7)) . words

toNode :: Char -> Node
toNode = subtract 4 . ord
fromNode :: Node -> Char
fromNode = chr . (+ 4)

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

timedTraverseGraph :: Gr Char () -> Int
timedTraverseGraph gr =
  either id id
    . flip evalState (S gr mempty 0)
    . runExceptT
    $ timedTraverseGraph'

timedTraverseGraph' :: ExceptT Int (State S) a
timedTraverseGraph' = do
  S gr inProgress time <- get
  let availableNodes =
        filter (`M.notMember` inProgress)
          . nodes
          . nfilter ((== 0) . indeg gr)
          $ gr
  let freeWorkers = 5 - length inProgress
  let newJobs     = take freeWorkers (sort availableNodes)
  jobs <>= (M.fromList . fmap (id &&& id) $ newJobs)
  timer += 1
  jobs . traversed -= 1
  finishJobs
  when (isEmpty gr && null inProgress) (throwError time)
  timedTraverseGraph'
 where
  finishJobs = do
    (finishedJobs, unfinishedJobs) <- uses jobs (M.partition (<= 0))
    graph %= delNodes (M.keys finishedJobs)
    jobs .= unfinishedJobs
