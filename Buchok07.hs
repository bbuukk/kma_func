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
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr st_n fn_n
    | not (isGraph gr) = Nothing
    | otherwise = result (st_n : check1 gr (gr!!st_n) fn_n) fn_n

check1 gr cr_n fn_n = longest
     [if next == fn_n
         then [next]
         else next : (check1 gr (gr!!next) fn_n) | next <- cr_n]

result res fn_nd
        | last res /= fn_nd = Nothing
        |otherwise = Just res

--todo Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay nds
            | not (isGraph nds) = Nothing
            | otherwise = Just (head ([ foo nds (nds!!idx) [idx]| idx <- idxs nds] ))

foo ns c_n res
        | is_cycle res = res
        | length res > ((length ns) + 1) = []
        | otherwise = head1 (filter (is_cycle) (filter (\cls -> length cls == ((length ns) + 1)) [ foo ns (ns!!n) (res++[n])| n <- reverse c_n]))


--todo Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr | not (isGraph gr) = False
             | otherwise = check2 (buildTransitive gr)
                where check2 t_gr = null[ t |idx <- idxs t_gr, let t = t_gr!!idx, idx `elem` t]

--todo Задача 8 ------------------------------------
-- topolSort :: Graph -> Maybe [Int] 
-- topolSort gr | not (isGraph gr) || not (isAcyclic gr) = []
--              | otherwise = 

final_c gr = foldl (\acc pair -> ins acc pair) [0,1,2,3] (foo1 gr)

foo1 gr = [ [idx, i_c_n] | idx <- idxs gr, let c_n = gr!!idx, i_c_n <- c_n]

-- ins [] [n1,n2] = [n1,n2]
ins e_l@(e:es) n_e@[n1,n2]
    | n2 `notElem` e_l = b_ins (break (==n1) e_l) n_e
    | otherwise        = n_ins (break (==n2) e_l) n_e
    -- |n2<e = n1:e:es
    -- |otherwise = e:ins es n_e 

b_ins (p1, (p:p2)) [n1,n2]
            | n1 == p   = p1 ++ p : p2
            | otherwise = p1 ++ p : n2 : p2 

n_ins (p1, (p:p2)) [n1,n2] =  p1 ++ p : n1 : p2
            


--todo Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

---------------------Допоміжні функції ----------
longest [] = []
longest as = maximumBy (comparing length) as

is_cycle [] = False
is_cycle (a1:[]) = False
is_cycle (a1:as) = a1 == last as


sort_insert :: (Ord a, Eq a) => [a] -> a -> [a]
sort_insert [] b = [b]
sort_insert (a:as) b
    |b<a = b:a:as
    |otherwise = a:sort_insert as b         

make_unique :: (Ord a, Eq a) => [a] -> [a]
make_unique [] = []
make_unique (x:xs) = x : make_unique (filter(/= x) xs)

idxs :: Graph -> [Int]
idxs g = [0..(length g - 1)]

head1 [] = []
head1 as = head as