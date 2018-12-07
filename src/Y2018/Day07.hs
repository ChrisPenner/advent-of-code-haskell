module Y2018.Day07 where

import Data.Char
import Data.List
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Data.Graph.Inductive

type Time = Int

loadInput :: IO [String]
loadInput = lines . T.unpack . T.strip <$> TIO.readFile "./input/2018-07.txt"

part1 :: IO ()
part1 = do
  gr <- parseGraph <$> loadInput
  print . fmap fromNode $ unfoldGraph gr

part2 :: IO ()
part2 = do
  gr <- parseGraph <$> loadInput
  print $ timeGraph (gr, mempty)

parseGraph :: [String] -> Gr Char ()
parseGraph instructions = mkGraph nodes' edges'
 where
  allLabels        = S.fromList $ foldMap (\(a, b) -> [a, b]) edgeLabels
  nodes'           = (toNode &&& id) <$> (toList allLabels)
  edges' = (\(src, dest) -> (toNode src, toNode dest, ())) <$> edgeLabels
  edgeLabels       = parseInstruction <$> instructions
  parseInstruction = (head . (!! 1) &&& head . (!! 7)) . words

-- We use the time of each node as its ID
toNode :: Char -> Node
toNode = subtract 4 . ord
fromNode :: Node -> Char
fromNode = chr . (+ 4)

unfoldGraph :: Gr Char () -> [Node]
unfoldGraph = unfoldr coalg
 where
  coalg gr | isEmpty gr = Nothing
  coalg gr =
    let availableNodes = nodes . nfilter ((== 0) . indeg gr) $ gr
        currentNode    = minimum availableNodes
    in  Just (currentNode, delNode currentNode gr)

timeGraph :: (Gr Char (), M.Map Node Time) -> Int
timeGraph = length . unfoldr coalg
 where
  coalg (gr, _) | isEmpty gr = Nothing
  coalg (gr, inProgress) =
    let availableNodes =
          filter (`M.notMember` inProgress)
            . nodes
            . nfilter ((== 0) . indeg gr)
            $ gr
        freeWorkers = 5 - length inProgress
        newJobs     = take freeWorkers (sort availableNodes)
        inProgressWithNewJobs =
          inProgress `mappend` (M.fromList . fmap (id &&& id) $ newJobs)
        jobsReducedTime                = subtract 1 <$> inProgressWithNewJobs
        (finishedJobs, unfinishedJobs) = M.partition (<= 0) jobsReducedTime
    in  Just ((), (delNodes (M.keys finishedJobs) gr, unfinishedJobs))
