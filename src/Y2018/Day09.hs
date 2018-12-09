module Y2018.Day09 where

import Data.Traversable as T
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Sequence as S

players, numMarbles1, numMarbles2 :: Int
players = 405
numMarbles1 = 70953
numMarbles2 = 7095300

part1 :: IO ()
part1 = print . solve $ numMarbles1

part2 :: IO ()
part2 = print . solve $ numMarbles2

type MarbleRing = S.Seq Int

solve :: Int -> Int
solve =
  getSum
    . maximum
    . M.unionsWith mappend
    . toList
    . snd
    . calcScores
    . mkMarbles

calcScores :: MarbleRing -> (MarbleRing, S.Seq (M.Map Int (Sum Int)))
calcScores = T.mapAccumL oneStep (S.singleton 0)


oneStep :: MarbleRing -> Int -> (MarbleRing, M.Map Int (Sum Int))
oneStep ring n | (n `mod` 23) == 0 =
  let (before, p S.:<| after) = S.splitAt (S.length ring - 7) ring
  in  (after <> before, M.singleton playerNum (Sum (p + n)))
  where playerNum = n `mod` players
oneStep ring n =
  let (before, after) = S.splitAt 2 ring in (n S.:<| after <> before, mempty)

mkMarbles :: Int -> S.Seq Int
mkMarbles n = S.drop 1 $ S.fromFunction (n + 1) id
