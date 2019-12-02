{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Y2019.Day02 where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text.Lens
import Data.Text.IO as TIO
import Data.Map.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State

-- solve1 :: IO ()
-- solve1 = do
--     input <- TIO.readFile "./src/Y2019/day02.txt"
--     print $ input
--             & toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
--             & ix 1 .~ 12
--             & ix 2 .~ 2
--             & (,) 0
--             &~ fix (\continue -> do
--                 let loadRegister r = use (_2 . singular (ix r))
--                 let loadNext = _1 <<+= 1 >>= loadRegister
--                 let getArg = loadNext >>= loadRegister
--                 out <- getOp <$> loadNext <*> getArg <*> getArg
--                 outputReg <- loadNext
--                 _2 . ix outputReg .= out
--                 use _1 >>= loadRegister >>= \case
--                   99 -> return ()
--                   _ -> continue
--                 )
--             & view (_2 . singular (ix 0))

getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+)
getOp 2 = (*)
getOp n = error $ "unknown op-code: " <> show n

solveSingle :: M.Map Int Int -> Int -> Int -> Int
solveSingle registers noun verb =
    registers
    & ix 1 .~ noun
    & ix 2 .~ verb
    & (,) 0
    &~ fix (\continue -> do
        let loadRegister r = use (_2 . singular (ix r))
        let loadNext = _1 <<+= 1 >>= loadRegister
        let getArg = loadNext >>= loadRegister
        out <- getOp <$> loadNext <*> getArg <*> getArg
        outputReg <- loadNext
        _2 . ix outputReg .= out
        use _1 >>= loadRegister >>= \case
          99 -> return ()
          _ -> continue
        )
    & view (_2 . singular (ix 0))

solvePart2 :: IO ()
solvePart2 = do
    registers <- TIO.readFile "./src/Y2019/day02.txt"
               <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))
    print $ findIndexOf  ( traversed 
                         . reindexed (\(noun, verb) -> (100 * noun) + verb) selfIndex 
                         . to (uncurry (solveSingle registers)))
            (== 19690720)
            [(noun, verb) | noun <- [0..99], verb <- [0..99]]

-- solve1 :: IO ()
-- solve1 = do
--     input <- TIO.readFile "./src/Y2019/day02.txt"
--     let registers = toMapOf ([regex|\d+|] <. (match . unpacked . _Show @Int)) input
--                       & ix 1 .~ 12
--                       & ix 2 .~ 2
--     let result :: Maybe (M.Map Int Int) = lastOf (unfolded computeStep) (0, registers)
--     print $ result ^.. _Just . traversed
--     -- print $ toListOf (taking 10  (iterated computeStep)) (0, registers)
--     return ()
--   where
--     computeStep :: (Int, M.Map Int Int) -> Maybe (M.Map Int Int, (Int, M.Map Int Int))
--     computeStep (step, m) =
--         case step ^.. taking 4 (iterated (+1)) . to (flip M.lookup m) . _Just of
--             (99:_) -> Nothing
--             [opCode, a, b, out] ->
--                 let resultM = m & at out ?~ getOp opCode (m ^?! ix a) (m ^?! ix b)
--                  in Just (resultM, (step + 4, resultM))
--             _ -> error "unexpected case"

-- solve2 :: IO ()
-- solve2 = do
--     input <- TIO.readFile "./src/Y2019/day02.txt"
--     let (n,v) = head $ do
--         noun <- [0..99]
--         verb <- [0..99]
--         let registers = toMapOf ([regex|\d+|] <. (match . unpacked . _Show @Int)) input
--                         & ix 1 .~ noun
--                         & ix 2 .~ verb
--         let Just result :: Maybe (M.Map Int Int) = lastOf (unfolded computeStep) (0, registers)
--         let output = result ^?! ix 0
--         guard (output == 19690720)
--         return (noun, verb)

--     print $ (100 * n) + v
--   where
--     computeStep :: (Int, M.Map Int Int) -> Maybe (M.Map Int Int, (Int, M.Map Int Int))
--     computeStep (step, m) =
--         case step ^.. taking 4 (iterated (+1)) . to (flip M.lookup m) . _Just of
--             (99:_) -> Nothing
--             [opCode, a, b, out] ->
--                 let resultM = m & at out ?~ getOp opCode (m ^?! ix a) (m ^?! ix b)
--                  in Just (resultM, (step + 4, resultM))
--             _ -> error "unexpected case"

-- solved :: IO ()
-- solved = do
--     input <- TIO.readFile "./src/Y2019/day02.txt" <&> toListOf ([regex|\d+|] <. (match . unpacked . _Show @Int))
--     print $ solveLensy 12 2 input

-- main :: IO ()
-- main = do
--     input <- TIO.readFile "./src/Y2019/day02.txt"
--                <&> toListOf ([regex|\d+|] <. (match . unpacked . _Show @Int))
--     print $ solve2Lensy input

-- solveLensy :: Int -> Int -> [Int] -> Int
-- solveLensy noun verb input =
--     toMapOf itraversed input
--       & ix 1 .~ noun
--       & ix 2 .~ verb
--       & (,) 0
--       & until (\(stepper, reg) -> anyOf (ix stepper) (== 99) reg)
--           (execState $ do
--             let loadRegister r = use (_2 . singular (ix r))
--             let loadNext = _1 <<+= 1 >>= loadRegister
--             let getArg = loadNext >>= loadRegister
--             result <- getOp <$> loadNext <*> getArg <*> getArg
--             outputReg <- loadNext
--             _2 . ix outputReg .= result
--          )
--       & view (_2 . singular (ix 0))

-- solve2Lensy :: [Int] -> Maybe Int
-- solve2Lensy input =
--     () ^?
--       ((taking 100 (indexing repeated) <.> taking 100 (indexing repeated))
--       . asIndex
--       . to (\(noun, verb) -> solveLensy noun verb input)
--       . only 19690720 . asIndex
--       . to (\(noun, verb) -> 100 * noun + verb))
