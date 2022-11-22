{-# OPTIONS_GHC -Wall #-}
module Buchok07 where

-- import Data.Maybe
import Data.Ord
import Data.List

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


--todo Задача 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive orgr = isGraph orgr && check orgr 
        where
            check [] = True
            check gr = and [ t_n `elem` g | g<-gr, node <- g, let t_nodes = orgr!!node, t_n <- t_nodes]

--todo Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph
buildTransitive nds
            | not (isGraph nds) = []
            | otherwise = [ build nds node node| node <- nds ]

build [] _ res = res
build nds new_node res = continue nds g_new res
    where
         g_new = foldl (find_new_nodes) [] new_node
         find_new_nodes acc nd =
             [ n | n <- nds!!nd, n `notElem` res, n `notElem` acc] ++ acc

continue _ [] res         = build [] [] res
continue nds new_node res = build nds new_node (foldl (sort_insert) res new_node)

--todo Задача 5 ------------------------------------
-- longWay :: Graph -> Int -> Int -> Maybe [Int] 
-- longWay gr st_nd fn_nd 
--     | not (isGraph gr) = Nothing
--     | otherwise = foo gr st_nd fn_nd

--tod check if current node has fn_n
-- foo gr st_n fn_n = [ st_n : next : check1 gr next fn_n | next <- gr!!st_n]
-- check1 gr cr_n fn_n = longest [ if next == fn_n then [next] else next : (check1 gr next fn_n) | next <- gr!!cr_n]

foo gr st_n fn_n = st_n : check1 gr (gr!!st_n) fn_n 
check1 gr cr_n fn_n = longest [ if next == fn_n then [next] else next : (check1 gr (gr!!next) fn_n) | next <- cr_n]
-- gr3 = [[1],[2],[3],[1],[0,3]]
-- gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

longest = maximumBy (comparing length)

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


-- insert :: [Integer] -> Integer -> [Integer]
sort_insert [] b = [b]
sort_insert (a:as) b
    |b<a = b:a:as
    |otherwise = a:sort_insert as b

-- sortInsert :: [Integer] -> [Integer]
-- sortInsert as = foldl insert [] as 
--     where
         

set :: [Int] -> [Int]
set [] = []
set (x:xs) = x : set (filter(/= x) xs)

-- sortInsert :: [Int] -> [Int]
-- sortInsert [] = []
-- sortInsert [x] = [x]
-- sortInsert (x:xs) = insert (sortInsert xs) x
--           where insert :: [Int] -> Int -> [Int]
--                 insert [] n = [n]
--                 insert ys n | n > head ys = head ys : insert (tail ys) n
--                             | otherwise = n : ys
     
