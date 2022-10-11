{-# OPTIONS_GHC -Wall #-}
module Buchok02 where

--todo Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr as = foldr (+) 0 as
  
--todo Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial a
    |a < 0  = -1
    |a == 0 =  1
    |True  =  foldl (*) 1 [1..a]

--todo Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr as bs = foldr (:) bs as

--todo Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert as =
     let insert :: [Integer] -> Integer -> [Integer]
         insert [] b = [b]
         insert (a:as) b
            |b<a = b:a:as
            |otherwise = a:insert as b
     in foldl insert [] as 

--todo Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 (f) xs ys

--todo Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart a b = sum [(fromIntegral(a)^i)/fromIntegral(factorial i) | i <- [1..b]]

--todo Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

--todo Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [x*x|x<-[1..]]

--todo Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys= [ i | i <- [0.. length ys], xs == take (length xs) (drop i ys) ]

