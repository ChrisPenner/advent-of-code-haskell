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
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Lens
import Control.Monad.Except
import Data.Graph.Inductive


loadInput :: IO [String]
loadInput = lines . T.unpack . T.strip <$> TIO.readFile "./input/2018-07.txt"

part1 :: IO ()
part1 = do
  gr <- parseGraph <$> loadInput
  print $ traverseGraph gr

part2 :: IO ()
part2 = do
  gr <- parseGraph <$> loadInput
  timedTraverseGraph gr >>= print

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

timedTraverseGraph :: Gr Char () -> IO Int
timedTraverseGraph gr =
  let ioList =
        flip evalStateT (gr, mempty, mempty, 0)
          . runExceptT
          $ timedTraverseGraph'
  in  either id id <$> ioList

type Time = Int
timedTraverseGraph'
  :: ExceptT Int (StateT (Gr Char (), M.Map Node Time, [Node], Int) IO) a
timedTraverseGraph' = do
  get
    >>= liftIO
    .   print
    .   (\(_, b, c, _) -> (M.mapKeys fromNode b, fmap fromNode c))
  tickTime
  finishJobs
  (gr, inProgress, result, timer) <- get
  when (isEmpty gr && null inProgress) (throwError timer)
  _4 += 1
  let availableNodes =
        filter (`M.notMember` inProgress)
          . nodes
          . nfilter ((== 0) . indeg gr)
          $ gr
  let freeWorkers = 5 - length inProgress
  let newJobs     = take freeWorkers (sort availableNodes)
  _2 <>= (M.fromList . fmap (id &&& id) $ newJobs)
  timedTraverseGraph'
 where
  tickTime   = _2 . traversed -= 1
  finishJobs = do
    (finishedJobs, unfinishedJobs) <- uses _2 (M.partition (<= 0))
    _1 %= delNodes (M.keys finishedJobs)
    _3 <>= (sort $ M.keys finishedJobs)
    _2 .= unfinishedJobs
