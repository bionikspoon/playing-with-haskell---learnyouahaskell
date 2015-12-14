module Hof where

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithOneHundred :: (Num a, Ord a) => a -> Ordering
compareWithOneHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (x:xs)
    | predicate x = x : filter' predicate xs
    | otherwise = filter' predicate xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n = n : collatz (n * 3 + 1)


numLongChains :: Int
numLongChains = length $ filter isLong $ map collatz [1..10]
    where
        isLong xs = length xs > 15
