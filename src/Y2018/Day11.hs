{-# LANGUAGE ViewPatterns #-}
module Y2018.Day11 where

import Data.Grid
import Control.Comonad.Representable.Store
import Data.Functor.Rep
import Data.Maybe
import Data.Function
import Control.Lens
import Control.Comonad
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Functor.Compose
import Data.Foldable

gridSerial :: Int
gridSerial = 2694

gridSize :: Int
gridSize = 300

type GridSize = 300


grid :: Store (Grid GridSize GridSize) Int
grid = store go (0, 0)
 where
  go (succ -> x, succ -> y) = powerLevel''
   where
    rackID     = x + 10
    powerLevel = ((rackID * y) + gridSerial) * rackID
    powerLevel' =
      fromMaybe 0 $ powerLevel ^? (to show . reversed . ix 2 . to (read . pure))
    powerLevel'' = powerLevel' - 5

unStore :: Representable r => Store r a -> r a
unStore = tabulate . fst . runStore

findMax :: Store (Grid GridSize GridSize) Int -> (Int, Int)
findMax =
  (succ *** succ)
    . fromMaybe (-1, -1)
    . fmap fst
    . maximumByOf (itraversed . withIndex) (compare `on` snd)
    . unStore
    . extend (sum . experiment toSquare)
 where
  toSquare (x, y) = liftA2 (,) (restrict [x .. x + 2]) (restrict [y .. y + 2])
  restrict = filter (\x -> x >= 0 && x < gridSize)

maxOfAllSquares :: [[Int]] -> (Int, Int)
maxOfAllSquares xs =
  maximumBy (compare `on` snd)
    $ zip [0 ..]
    . scanl (\acc n -> sum n + acc) 0
    $ xs

findMaxAnySquare
  :: Store (Grid GridSize GridSize) Int -> ((Int, Int), (Int, Int))
findMaxAnySquare =
  first (succ *** succ)
    . fromMaybe (error "something went wrong")
    . maximumByOf (itraversed . withIndex) (compare `on` snd . snd)
    . unStore
    . extend (maxOfAllSquares . getCompose . experiment (Compose . toSquare))

toSquare (x, y) = filter
  (not . null)
  ([[(x, y)]] ++ fmap buildSquare [1 .. (gridSize - 1)])
 where
  buildSquare size = do
    let vertical   = (, y + size) <$> [x .. x + size]
    let horizontal = (x + size, ) <$> [y .. y + size]
    filter (\(x', y') -> (x' < gridSize) && (y' < gridSize))
           (vertical ++ horizontal)
