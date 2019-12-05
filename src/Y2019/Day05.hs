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
import Control.Monad.Writer
import Control.Monad.Reader

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

loop :: Computer ()
loop = do
    step >>= \case
      Halt -> return ()
      Continue -> loop

data Status = Halt | Continue

step :: Computer Status
step = do
    opCode <- loadNext
    case splitOp opCode of
        (modes, 1) -> binOp (+) modes
        (modes, 2) -> binOp (*) modes
        ([_, _, _], 3) -> do
            outAddress <- loadWithMode 1
            ask >>= store outAddress
            return Continue
        ([_, _, a], 4) -> do
            result <- loadWithMode a
            tell [result]
            return Continue
        ([_, b, a], 5) -> do
            cond <- loadWithMode a
            pos <- loadWithMode b
            case cond of
                0 -> return ()
                _ -> _1 .= pos
            return Continue
        ([_, b, a], 6) -> do
            cond <- loadWithMode a
            pos <- loadWithMode b
            case cond of
                0 -> _1 .= pos
                _ -> return ()
            return Continue
        (modes, 7) -> binOp (\a b -> view (from enum) (a < b)) modes
        (modes, 8) -> binOp (\a b -> view (from enum) (a < b)) modes
        (_, 99) -> do
            return Halt

binOp :: (Int -> Int -> Int) -> [Int] -> Computer Status
binOp f [_, b, a] = do
    first <- loadWithMode a
    second <- loadWithMode b
    pos <- loadWithMode 1
    store pos (f first second)
    return Continue

solve :: Int -> IO ()
solve inp = do
    registers <- TIO.readFile "./src/Y2019/day05.txt"
               <&> toMapOf (indexing ([regex|-?\d+|] . match . unpacked . _Show @Int))
    w <- flip evalStateT (0, registers) . execWriterT . flip runReaderT inp $ loop
    print w
