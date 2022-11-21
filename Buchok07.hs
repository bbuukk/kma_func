{-# OPTIONS_GHC -Wall #-}
module Buchok07 where

-- import Data.List
-- import Data.Maybe

type Graph  = [[Int]]

xor :: Eq a => a -> a -> Bool
xor a b = a /= b

--todo Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph []     = True
isGraph (g:gr) = allDifferent g && isGraph gr

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

--todo Задача 2 ------------------------------------
isTournament :: Graph -> Bool 
isTournament orgr = isGraph orgr && check orgr 0
        where
            check [] _ = True
            check (g:gr) curr_node = 
                and [(node `elem` g) `xor` (curr_node `elem` (orgr!!node)) 
                    | node <- [0..(length orgr - 1)], node /= curr_node] 
                        && check gr (curr_node + 1)

foo1, foo2:: Graph
foo1 = [[1,2,3],[2,3],[3,4],[4],[]]
foo2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]

--todo Задача 3 ------------------------------------
-- isTransitive :: Graph -> Bool 
-- isTransitive orgr = isGraph orgr && check orgr 0
--         where
--             check [] _ = True
--             check (g:gr) = 
--                 and [ g `contain all nodes from` (orgr!!node)) 
--                     | node <- g] 
--                         && check gr (curr_node + 1)

-- gr3 =  
-- gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

--todo Задача 4 ------------------------------------
-- buildTransitive :: Graph -> Graph
-- buildTransitive nodes
--             | not (isGraph gr) = []
--             | otherwise = scanr (:.build) [] nds

-- build _ [] = []
-- build nds node = [] ++ build nds (if null (count nds node) then [] else node ++ (count nds node))

build [] node  = node
build nds node = continue nds node (count nds node)

continue nds node new_node
        | null new_node = build [] node
        | otherwise     = build nds (node ++ new_node)

count nds node = foldl (\acc nd -> [ n | n <- nds!!nd, n `notElem` node, n `notElem` acc] ++ acc) [] node

--todo Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay = undefined

-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay = undefined

-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic = undefined

-- Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort = undefined

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

---------------------Допоміжні функції ----------

-- sortInsert :: [Integer] -> [Integer]
-- sortInsert as = foldl insert [] as 
--     wher insert :: [Integer] -> Integer -> [Integer]
--          insert [] b = [b]
--          insert (a:as) b
--             |b<a = b:a:as
--             |otherwise = a:insert as b
     
