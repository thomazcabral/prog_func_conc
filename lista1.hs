import Data.List (sort)
import Data.Char (toUpper)
import Data.List (intercalate)

countSorted :: [String] -> Int
countSorted xs = length (filter (\s -> s == sort s) xs)

mStr :: [String] -> String
mStr = intercalate " " . map (map toUpper) . filter (\x -> length x > 5)

sumsOf :: [Integer] -> [Integer]
sumsOf [] = []
sumsOf [a, b] = [a, a+b]
sumsOf xs = sumsOf (take (n-1) xs) ++ [(sum xs)]
    where
        n = length xs

count :: Integer -> [Integer] -> Integer
count i [] = 0
count i (x:xs) = if i == x then 1 + (count i xs) else (count i xs)

unique :: [Integer] -> [Integer]
unique xs = [x | x <- xs, count x xs == 1]