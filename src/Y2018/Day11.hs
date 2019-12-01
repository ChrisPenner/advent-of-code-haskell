{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Y2018.Day11 where

-- import Data.Grid hiding (gridSize)
-- import Control.Comonad.Representable.Store
-- import Data.Functor.Rep
-- import Data.Maybe
-- import Data.Function
-- import Control.Lens
-- import Control.Comonad
-- import Control.Arrow
-- import Control.Applicative
-- import Data.Functor.Compose
-- import Data.Foldable

gridSerial :: Int
gridSerial = 2694

gridSize :: Int
gridSize = 300

type GridSize = 300


-- grid :: Store (Grid '[GridSize, GridSize]) Int
-- grid = store go (Coord [0, 0])
--  where
--   go :: Coord '[GridSize, GridSize] -> Int
--   go (Coord [succ -> x, succ -> y]) = powerLevel''
--    where
--     rackID     = x + 10
--     powerLevel = ((rackID * y) + gridSerial) * rackID
--     powerLevel' =
--       fromMaybe 0 $ powerLevel ^? (to show . reversed . ix 2 . to (read . pure))
--     powerLevel'' = powerLevel' - 5

-- unStore :: Representable r => Store r a -> r a
-- unStore = tabulate . fst . runStore

-- fromTup :: (Int, Int) -> Coord '[GridSize, GridSize]
-- fromTup (x, y) = Coord [x, y]

-- toTup :: Coord '[GridSize, GridSize] -> (Int, Int)
-- toTup (Coord [x, y]) = (x, y)

-- findMax :: Store (Grid '[GridSize, GridSize]) Int -> Coord '[GridSize, GridSize]
-- findMax =
--     fromTup
--     . _ -- (succ *** succ)
--     . fromMaybe (fromTup (-1, -1))
--     . fmap fst
--     . maximumByOf (itraverseRep . withIndex) (compare `on` snd)
--     . unStore
--     . extend (sum . experiment toSquare')
--  where
--   toSquare' :: Coord '[GridSize, GridSize] -> [Coord '[GridSize, GridSize]]
--   toSquare' (Coord [x, y]) = do
--       a <- (restrict [x .. x + 2])
--       b <- (restrict [y .. y + 2])
--       return (Coord [a, b])
--   restrict :: [Int] -> [Int]
--   restrict = filter (\x -> x >= 0 && x < gridSize)

-- maxOfAllSquares :: [[Int]] -> (Int, Int)
-- maxOfAllSquares xs =
--   maximumBy (compare `on` snd)
--     $ zip [0 ..]
--     . scanl (\acc n -> sum n + acc) 0
--     $ xs

-- findMaxAnySquare
--   :: Store (Grid '[GridSize, GridSize]) Int -> ((Int, Int), (Int, Int))
-- findMaxAnySquare =
--   first (succ *** succ)
--     . fromMaybe (error "something went wrong")
--     . maximumByOf (itraversed . withIndex) (compare `on` snd . snd)
--     . unStore
--     . extend (maxOfAllSquares . getCompose . experiment (Compose . toSquare))

-- toSquare (x, y) = filter
--   (not . null)
--   ([[(x, y)]] ++ fmap buildSquare [1 .. (gridSize - 1)])
--  where
--   buildSquare size = do
--     let vertical   = (, y + size) <$> [x .. x + size]
--     let horizontal = (x + size, ) <$> [y .. y + size]
--     filter (\(x', y') -> (x' < gridSize) && (y' < gridSize))
--            (vertical ++ horizontal)
