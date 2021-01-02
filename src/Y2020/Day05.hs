module Y2020.Day05 where

import Data.List ( sort )
import Control.Lens ( (&), (.@~), taking )
import Data.Bits.Lens ( bits )
import qualified Data.Set as S

main :: IO ()
main = do
    passes <- fmap toNumber . lines <$> readFile "./day05.txt"
    -- part 1
    print . maximum $ passes
    -- part 2
    print . findMissing' $ passes

findMissing :: [Int] -> Maybe Int
findMissing (x:y:rest)
  | x + 2 == y = Just $ x + 1
  | otherwise = findMissing (y:rest)
findMissing _ = Nothing

findMissing' :: [Int] -> Maybe Int
findMissing' xs = 
    let (candidates1, candidates2, exclusions) = foldMap toSets xs
     in S.lookupMin $ (candidates1 `S.intersection` candidates2) S.\\ exclusions
  where
    toSets n = (S.fromList [n-1], S.fromList [n+1], S.singleton n)

toNumber :: String -> Int
toNumber s = 0 & taking 10 bits .@~ (\i -> elem (reverse s !! i) "BR")

-- toNumber :: String -> Int
-- toNumber = foldl go 0 . zip [0..] . reverse
--   where
--     go :: Int -> (Int, Char) -> Int
--     go n (i,'F') = clearBit n i
--     go n (i,'B') = setBit n i
--     go n (i,'L') = clearBit n i
--     go n (i,'R') = setBit n i
--     go _ c = error (show c)
