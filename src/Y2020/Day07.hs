{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2020.Day06 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Graph
import Control.Lens.Regex.Text
import Control.Lens

splitter :: T.Text -> (T.Text, [T.Text])
splitter = \case
  (preview ([regex|^(\w+ \w+) bags contain no other bags|] . group 0) -> Just key) -> (key, [])
  (view ([regex|^(\w+ \w+) bags contain (.+)$|] . groups) -> [key, inner]) -> (key, inner ^. [regex|(\w+ \w+) bag|] . groups)
  txt -> error $ T.unpack txt <> " didn't parse!"

main :: IO ()
main = do
    entries <- fmap T.lines . T.readFile $ "./day07.txt"
    let containsMap :: M.Map T.Text [T.Text]
        containsMap = M.unionsWith (<>) . fmap (uncurry M.singleton) . fmap splitter $ entries
        numberedVertices :: [(Vertex, T.Text, [T.Text])]
        numberedVertices = zipWith (\n (k, vs) -> (n, k, vs)) [(0::Vertex)..] $ M.toList containsMap
        (g, lookupNode, lookupVertex) = graphFromEdges numberedVertices
        Just shinyVertex = lookupVertex "shiny gold"
        (nonShinyVertices :: [Vertex]) = filter (/= shinyVertex) . vertices $ g
    -- Check which bag types have a path to them from the shiny bag
    print . length .  fmap lookupNode $ filter (flip (path g) shinyVertex) nonShinyVertices
