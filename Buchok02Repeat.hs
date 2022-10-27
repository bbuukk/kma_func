{-# OPTIONS_GHC -Wall #-}
module Buchok02Repeat where

-- Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr  = foldr (+) 0
  
-- Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

-- Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr [] _  = []
concatFr _ []  = []
concatFr as bs = foldr (:) bs as  

-- Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert as = 
    let insert :: [Integer] -> Integer -> [Integer]
        insert [] b = [b]
        insert (a:as) b
            | b<a       = b:a:as
            | True = a:insert as b
    in foldl insert [] as 

-- Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (a:as) (b:bs) = (f) a b : map2 (f) as bs  

-- Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart = undefined

-- Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = undefined

-- Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = undefined

-- Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes = undefined

