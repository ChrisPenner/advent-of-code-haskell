module Y2018.Day08 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree
import Data.Maybe

type Parser = Parsec () String
type Meta = [Int]

loadInput :: IO (Tree Meta)
loadInput = do
  mTree <- parseMaybe parseNode . T.unpack . T.strip <$> TIO.readFile
    "./input/2018-08.txt"
  maybe (fail "bad parse") pure mTree

part1 :: IO ()
part1 = do
  print . sum . fmap sum =<< loadInput

part2 :: IO ()
part2 = do
  print . sumTree =<< loadInput

int :: Parser Int
int = read <$> many digitChar <* space

parseNode :: Parser (Tree Meta)
parseNode = do
  numChildren <- int
  numMeta     <- int
  children    <- count numChildren parseNode
  meta        <- count numMeta int
  return $ Node meta children

safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs = if n < length xs then Just (xs !! n) else Nothing

sumTree :: Tree Meta -> Int
sumTree (Node meta []) = sum meta
sumTree (Node meta children) =
  sum . fmap (fromMaybe 0 . (`safeIndex` childSums) . pred) $ meta
  where childSums = sumTree <$> children
