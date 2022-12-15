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
             | True = check2 (buildTransitive gr)
                where check2 t_gr = null[ t |idx <- idxs t_gr, let t = t_gr!!idx, idx `elem` t]

--todo Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int]
topolSort gr | not (isGraph gr) || not (isAcyclic gr) = Nothing
             | True = Just (help_tpl_sort gr (filter (\vert -> is_zero_in gr vert (nodes gr))
                                             (nodes gr)))

help_tpl_sort gr stack
             | length gr <= length stack = stack
             | True  = help_tpl_sort (repl_at gr [] (find_c_v gr))
                                     (set (stack ++ find_c_v gr))

is_zero_in :: Graph -> Int -> [Int] -> Bool
is_zero_in gr v vs 
            | null vs = True
            | edge_in gr (head vs, v) = False
            | True = is_zero_in gr v (tail vs)

find_c_v :: Graph -> [Int]
find_c_v gr = filter (\vert -> is_zero_in gr vert (nodes gr))
                     (nodes gr)

--todo Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort [] _ = False
isTopolSort _ [] = False
isTopolSort nds tpl | not (isGraph nds) || not (isAcyclic nds) = False
                     | length nds /= length tpl = False
                     | not (is_set tpl)         = False
                     | True                     = check_is_sorted nds tpl

check_is_sorted [[]] [x] = x == 0
check_is_sorted nds tpl
     | null tpl = True
     | check_edges nds (head tpl) (tail tpl) = check_is_sorted nds (tail tpl)
     | True     = False

check_edges nds v tpl | null tpl              = True
                         | edge_in nds
                             (head tpl, v)    = False
                         | otherwise          = check_edges nds v (tail tpl)

---------------------todo Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

---------------------todo Допоміжні функції ----------
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

set :: [Int] -> [Int]
set [] = []
set (x:xs) = x : set (filter(/= x) xs)

edge_in :: Graph -> (Int, Int) -> Bool
edge_in g (x,y) = y `elem` (g!!x)

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

is_set :: [Int] -> Bool
is_set [] = True
is_set [_] = True
is_set (x:xs) = notElem x xs && is_set xs

repl_elem_at :: Int -> a -> [a] -> [a]
repl_elem_at pos newElem ls | pos >= length ls = error "Index over bound"
                                | otherwise = help pos newElem ls 0
                                where help :: Int -> a -> [a] -> Int -> [a]
                                      help _ _ [] _ = []
                                      help position element (x:xs) counter | counter == position = element : xs
                                                                                  | otherwise = x : help position element xs (counter + 1)                                                   

repl_at :: Graph -> [Int] -> [Int] -> Graph
repl_at nds _ [] = nds
repl_at nds n_e ps 
            | null ps = nds
            | otherwise = repl_at (repl_elem_at (head ps) n_e nds) n_e (tail ps)