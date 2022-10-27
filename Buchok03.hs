{-# OPTIONS_GHC -Wall #-}
module Buchok03 where

-- Mastermind -----------------------------------------

-- Фішка може мати один з шести кольорів
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- Код - просто список фішок
type Code = [Peg]

-- Крок гри (Move) будує конструктор Move використовуючи код (Code) і два цілих;  
-- кількість повних і часткових відповідностей кода-пропозиції і шифру
data Move = Move Code Int Int deriving (Show, Eq)

-- Список містить всі різні допустимі кольори
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

--todo Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (c:cd) (p:pr)
    | c == p    = 1 + exactMatches cd pr
    | otherwise = exactMatches cd pr

--todo Задача 2 -----------------------------------------

countColors :: Code -> [Int]
countColors cd = 
    let count_in [] _ = 0
        count_in (c:cd) color
            | c == color = 1 + count_in cd color
            | True       = count_in cd color 
    in [count_in cd color| color <- colors]

--todo Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd pr = 
        let minimum _ []          = []
            minimum [] _          = []
            minimum (c:cd) (p:pr) = (min c p) : (minimum cd pr)
        in sum (minimum (countColors cd) (countColors pr))

--todo Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd pr = Move pr ex_matches hf_matches 
        where ex_matches = exactMatches cd pr
              hf_matches = (matches cd pr) - ex_matches

--todo Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move pr ex_matches hf_matches) cd = (Move pr ex_matches hf_matches) == (getMove cd pr)

--todo Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cs = filter (isConsistent mv) cs

--todo Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 1 = [[color] | color <- colors]
allCodes n =  concatMap (\cs -> [cs++[color] | color <- colors]) (allCodes (n-1))
   
--todo Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = solve1 cd allcds
        where allcds = (allCodes (length cd))
              solve1 _ [] = []
              solve1 cd [c] = [getMove cd c]
              solve1 cd (c:acd) = (getMove cd c): (solve1 cd (filterCodes (getMove cd c) acd))
              