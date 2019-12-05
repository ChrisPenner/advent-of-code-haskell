{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Y2019.Day05 where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text.Lens
import qualified Data.Text.IO as TIO
import Data.Map.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State
import Control.Applicative
import Debug.Trace
import Control.Monad.Writer
import Control.Monad.Reader

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

-- solveSingle :: M.Map Int Int -> Int -> Int -> Int
-- solveSingle registers noun verb =
--     registers
--     & ix 1 .~ noun
--     & ix 2 .~ verb
--     & (,) 0
--     &~ fix (\continue -> do
--         let loadRegister r = use (_2 . singular (ix r))
--         let loadNext = _1 <<+= 1 >>= loadRegister
--         let getArg = loadNext >>= loadRegister
--         out <- getOp <$> loadNext <*> getArg <*> getArg
--         outputReg <- loadNext
--         _2 . ix outputReg .= out
--         use _1 >>= loadRegister >>= \case
--           99 -> return ()
--           _ -> continue
--         )
--     & view (_2 . singular (ix 0))


splitOp :: Int -> ([Int], Int)
splitOp (show -> i) = (map (read . pure) $ take 3 padded, read . reverse . take 2 . reverse $ padded)
  where
    padded = reverse . getZipList $ ZipList (reverse i) <|> ZipList ['0','0','0','0','0']

type Computer a = ReaderT Int (WriterT [Int] (StateT (Int, M.Map Int Int) IO)) a

loadWithMode :: Int -> Computer Int
loadWithMode 0 = do
    r <- loadNext
    out <- loadRegister r
    liftIO . putStrLn $ "loaded positional value " <> show out <> " from register " <> show r
    return out
loadWithMode 1 = do
    i <- use _1
    r <- loadNext
    liftIO . putStrLn $ "loaded immediate value " <> show r <> " from register " <>  show i
    return r

loadRegister :: Int -> Computer Int
loadRegister r = use (_2 . singular (ix r))

loadNext :: Computer Int
loadNext = _1 <<+= 1 >>= loadRegister

store :: Int -> Int -> Computer ()
store address val = do
    liftIO . putStrLn $ "stored value " <> show val <> " at address " <> show address
    _2 . at address ?= val

runOp :: Computer ()
runOp = do
    opCode <- loadNext
    -- o <- use _1
    -- liftIO $ print (o, splitOp opCode)
    case splitOp opCode of
        ([_, b, a], 1) -> do
            result <- liftA2 (+) (loadWithMode a) (loadWithMode b)
            outAddress <- loadWithMode 1
            store outAddress result
            runOp
        ([_, b, a], 2) -> do
            result <- liftA2 (*) (loadWithMode a) (loadWithMode b)
            outAddress <- loadWithMode 1
            store outAddress result
            runOp
        ([_, _, _], 3) -> do
            outAddress <- loadWithMode 1
            ask >>= store outAddress
            runOp
        ([_, _, a], 4) -> do
            result <- loadWithMode a
            tell [result]
            liftIO . putStrLn $ "OUTPUT: " <> show result
            runOp
        ([_, b, a], 5) -> do
            cond <- loadWithMode a
            pos <- loadWithMode b
            case cond of
                0 -> return ()
                _ -> _1 .= pos
            runOp
        ([_, b, a], 6) -> do
            cond <- loadWithMode a
            pos <- loadWithMode b
            case cond of
                0 -> _1 .= pos
                _ -> return ()
            runOp
        ([_, b, a], 7) -> do
            first <- loadWithMode a
            second <- loadWithMode b
            pos <- loadWithMode 1
            if first < second
               then store pos 1
               else store pos 0
            runOp
        ([_, b, a], 8) -> do
            first <- loadWithMode a
            second <- loadWithMode b
            pos <- loadWithMode 1
            if first == second
               then store pos 1
               else store pos 0
            runOp
        (_, 99) -> do
            return ()

solve :: Int -> IO ()
solve inp = do
    registers <- TIO.readFile "./src/Y2019/day05.txt"
               <&> toMapOf (indexing ([regex|-?\d+|] . match . unpacked . _Show @Int))
    w <- flip evalStateT (0, registers) . execWriterT . flip runReaderT inp $ runOp
    print w
