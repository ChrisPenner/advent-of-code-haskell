{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2020.Day08 where

import Data.Functor ( ($>) )
import Control.Applicative ( Alternative((<|>)) )
import qualified Data.IntMap as IM
import Control.Lens hiding (Context)
import Control.Monad.State

data Instr = Nop Int | Acc Int | Jmp Int
    deriving (Eq, Ord, Show)

data Context = Context
    { _instructions :: IM.IntMap Instr
    , _acc :: Int
    , _canSwap :: Bool
    }
makeLenses ''Context

main :: IO ()
main = do
    instrs <- fmap (fmap parseInstr . lines) . readFile $ "./day08.txt"
    let numberedInstrs = IM.fromList $ zip [0..] instrs
    let startContext = Context numberedInstrs 0 False
    print . head . fmap (view acc . snd) . flip runStateT startContext $ looper 0
    print . head . fmap (view acc . snd) . filter ((== length instrs) . fst) . flip runStateT (startContext{_canSwap=True}) $ looper 0


parseInstr :: String -> Instr
parseInstr s = case words s of
    ["nop", n] -> (Nop $ read (dropWhile (=='+') n))
    ["acc", n] -> (Acc $ read (dropWhile (=='+') n))
    ["jmp", n] -> (Jmp $ read (dropWhile (=='+') n))
    xs -> error $ concat xs

runInstr :: (MonadState Context m, Alternative m) => Int -> Instr -> m Int
runInstr i = \case
  (Acc n) -> (acc += n) $> succ i
  (Nop n) ->
    pure (succ i) <|> do
        canSwap <<.= False >>= guard
        pure (i + n)
  (Jmp n) ->
    pure (i + n) <|> do
        canSwap <<.= False >>= guard
        pure (succ i)


looper :: Int -> StateT Context [] Int
looper i = do
  use (instructions . at i) >>= \case
    Just instr -> do
        instructions %= sans i
        nextInstruction <- runInstr i instr
        looper nextInstruction
    Nothing ->
        return i
