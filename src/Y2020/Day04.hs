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
import Control.Lens.Regex.Text ( group, regex )
import Data.Text.Lens ( unpacked )
import Data.Monoid ( All(All, getAll) )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor.Compose ( Compose(Compose) )

main :: IO ()
main = do
    rows <- T.lines <$> T.readFile "./day04.txt"
    let validPassports = catMaybes (allFields . parsePassport <$> grouper rows)
    -- Step one
    print . length $ validPassports
    -- Step two
    print . length . filter validate $ validPassports

grouper :: [T.Text] -> [T.Text]
grouper ("":rest) = "":grouper rest
grouper (a:rest) =
    case grouper rest of
        (b:r) -> (a <> " " <> b) : r
        [] -> [a]
grouper [] = []

data Passport f =
    Passport {
    byr :: f T.Text
  , iyr :: f T.Text
  , eyr :: f T.Text
  , hgt :: f T.Text
  , hcl :: f T.Text
  , ecl :: f T.Text
  , pid :: f T.Text
  } deriving
    ( Generic
    , FunctorB, TraversableB, ApplicativeB, ConstraintsB
    )

instance Alternative f => Semigroup (Passport f) where
  (<>) = bzipWith (<|>)

instance Alternative f => Monoid (Passport f) where
  mempty = bpure empty

parsePassport :: T.Text -> Passport Maybe
parsePassport =  bsequence $ Passport
    { byr = Compose $ preview ([regex|byr:(\S+)|] . group 0)
    , iyr = Compose $ preview ([regex|iyr:(\S+)|] . group 0)
    , eyr = Compose $ preview ([regex|eyr:(\S+)|] . group 0)
    , hgt = Compose $ preview ([regex|hgt:(\S+)|] . group 0)
    , hcl = Compose $ preview ([regex|hcl:(\S+)|] . group 0)
    , ecl = Compose $ preview ([regex|ecl:(\S+)|] . group 0)
    , pid = Compose $ preview ([regex|pid:(\S+)|] . group 0)
    }

allFields :: Passport Maybe -> Maybe (Passport Identity)
allFields = bsequence'

validators :: Passport Predicate
validators = Passport
    { byr = Predicate $
        anyOf ([regex|^(\d+)$|] . asInt) (\n -> n >= 1920 && n <= 2002)
    , iyr = Predicate $
        anyOf ([regex|^(\d+)$|] . asInt) (\n -> n >= 2010 && n <= 2020)
    , eyr = Predicate $
        anyOf ([regex|^(\d+)$|] . asInt) (\n -> n >= 2020 && n <= 2030)
    , hgt = Predicate $
        -- Try both, succeed if either succeeds
        anyOf ((([regex|^(\d+)in$|] . asInt . to (\h -> h >= 59 && h <= 76)) 
             <> ([regex|^(\d+)cm$|] . asInt . to (\h -> h >= 150 && h <= 193)))) id
    , hcl = Predicate $
        has ([regex|^#[0-9a-f]{6}$|])
    , ecl = Predicate $ (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    , pid = Predicate $ (==9) . T.length
    }
  where
    asInt = group 0 . unpacked . _Show @Int

validate :: Passport Identity -> Bool
validate =
    getAll . bfoldMap (All . getConst) . bzipWith runValidator validators
  where
    runValidator (Predicate p) (Identity s) = Const $ p s
