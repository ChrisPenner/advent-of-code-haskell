{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Y2020.Day04 where

import Control.Applicative (Alternative(empty, (<|>)) )
import Data.Maybe ( catMaybes )
import Data.Functor.Contravariant ( Predicate(..) )
import Data.Functor.Barbie
import GHC.Generics (Generic)
import Control.Lens
import Control.Lens.Regex.Text ( group, regex, match )
import Data.Text.Lens ( unpacked )
import Data.Monoid ( All(All, getAll) )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor.Compose ( Compose(Compose) )

main :: IO ()
main = do
    rows <- T.lines <$> T.readFile "./day04.txt"
    print . length . filter validators . grouper $ rows

grouper :: [T.Text] -> [T.Text]
grouper ("":rest) = "":grouper rest
grouper (a:rest) =
    case grouper rest of
        (b:r) -> (a <> " " <> b) : r
        [] -> [a]
grouper [] = []

validators :: T.Text -> Bool
validators = fmap and $ sequenceA
    [ anyOf ([regex|byr:(\d+)|] . asInt) (\n -> n >= 1920 && n <= 2002)
    ,   anyOf ([regex|iyr:(\d+)|] . asInt) (\n -> n >= 2010 && n <= 2020)
    ,   anyOf ([regex|eyr:(\d+)|] . asInt) (\n -> n >= 2020 && n <= 2030)
    ,   anyOf ([regex|hgt:(\d+)in|] . asInt) (\h -> h >= 59 && h <= 76)
    ,   anyOf ([regex|hgt:(\d+)cm|] . asInt) (\h -> h >= 150 && h <= 193)
    ,  anyOf ([regex|hcl:#[0-9a-f]{6}|] . match)  (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    , has [regex|pid:\d{9}\b|]
    ]
  where
    asInt = group 0 . unpacked . _Show @Int
