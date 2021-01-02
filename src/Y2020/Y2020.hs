module Y2020.Y2020 where
import Control.Monad
import Data.List

input :: [ Int ]
input = [1721
        , 979
        , 366
        , 299
        , 675
        , 1456
        ]

matchSum2 :: Int -> [Int] -> [Int]
matchSum2 goal xs = do
    (y:ys) <- tails xs
    z <- ys
    guard (y + z == goal)
    return (y * z)

matchSum3 :: Int -> [Int] -> [Int]
matchSum3 goal xs = do
    (y:ys) <- tails xs
    (z:zs) <- tails ys
    a <- zs
    guard (y + z + a == goal)
    return (y * z * a)

main :: IO ()
main = do
    nums <- fmap read . words <$> readFile "./src/Y2020/day01.txt"
    print $ matchSum2 2020 nums


main2 :: IO ()
main2 = do
    nums <- fmap read . words <$> readFile "./src/Y2020/day01.txt"
    print $ matchSum3 2020 nums

