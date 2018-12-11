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

gridSerial :: Int
gridSerial = 2694


grid :: Store (Grid 300 300) Int
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

findMax :: Store (Grid 300 300) Int -> (Int, Int)
findMax =
  (succ *** succ)
    . fromMaybe (-1, -1)
    . fmap fst
    . maximumByOf (itraversed . withIndex) (compare `on` snd)
    . unStore
    . extend (sum . experiment toSquare)
 where
  toSquare (x, y) = liftA2 (,) (restrict [x .. x + 2]) (restrict [y .. y + 2])
  restrict = filter (\x -> x >= 0 && x <= 299)
