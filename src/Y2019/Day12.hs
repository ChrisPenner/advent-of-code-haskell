{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Y2019.Day06 where

import Linear.V3
import Control.Comonad.Representable.Store
import Data.Functor.Rep
import qualified Data.Vector.Sized as SV
import Data.Maybe
import GHC.TypeLits
import Data.Finite
import Control.Comonad
import Control.Applicative
import Data.Function ((&))
import Data.Foldable

main :: IO ()
main = do
    -- print . render . extend updatePlanet $ sim
    print . systemEnergy . head . drop 1000 $ iterations
  where
    iterations = fmap render . iterate (extend updatePlanet) $ sim

systemEnergy :: Foldable f => f Planet -> Int
systemEnergy planets = sum $ do
    p <- toList planets
    let potential = sum . fmap abs . position $ p
    let kinetic  = sum . fmap abs . velocity $ p
    return $ kinetic * potential

render :: Representable f => Store f a -> f a
render = tabulate . fst . runStore


updatePlanet :: forall n. KnownNat n => Store (SV.Vector n) Planet -> Planet
updatePlanet s = s & do
    refPos :: V3 Int <- position . extract
    otherPlanetPositions :: [V3 Int] <- fmap position . experiment (const $ finites @n)
    let velAdjustments :: V3 Int
        velAdjustments = sum . fmap (liftA2 compareCoord refPos) $ otherPlanetPositions
    applyAcceleration velAdjustments . extract
    where
      compareCoord :: Int -> Int -> Int
      compareCoord ref other = signum $ other - ref
      applyAcceleration :: V3 Int -> Planet -> Planet
      applyAcceleration acc (Planet{position, velocity}) =
          let newVelocity = velocity + acc
           in Planet
               { position = position + newVelocity
               , velocity = newVelocity
               }

sim :: Store (SV.Vector 4) Planet
sim = store (index inp) 0


data Planet = Planet
    { position :: V3 Int
    , velocity :: V3 Int
    } deriving Show

inp :: SV.Vector 4 Planet
inp = fromJust . SV.fromList $
        [ (Planet (V3 1 4 4) (V3 0 0 0))
        , (Planet (V3 (-4) (-1) 19) (V3 0 0 0))
        , (Planet (V3 (-15) (-14) 12) (V3 0 0 0))
        , (Planet (V3 (-17) (1) 10) (V3 0 0 0))
        ]

inp' :: SV.Vector 4 Planet
inp' = fromJust . SV.fromList $
        [ (Planet (V3 (-1) 0 2) (V3 0 0 0))
        , (Planet (V3 (2) (-10) (-7)) (V3 0 0 0))
        , (Planet (V3 (4) (-8) 8) (V3 0 0 0))
        , (Planet (V3 (3) (5) (-1)) (V3 0 0 0))
        ]
