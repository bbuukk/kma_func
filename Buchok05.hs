{-# OPTIONS_GHC -Wall #-}
module Buchok05 where

import Data.Maybe
import Data.Ratio
import Data.List (sort)

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne]
                 deriving (Show, Eq)

--todo Задача 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne 
coef 0 _  = []
coef c p0 = [ (p1,p2*c) | p0@(p1,p2) <- p0]

--todo Задача 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add p1 p2 =
      let
            insert :: PolinomOne -> (Int, Rational) -> PolinomOne
            insert [] t = [t]
            insert p1@((pf1,ps1):ps) p2@(pf2, ps2)
                  |(pf2 < pf1) = p2:p1
                  |(pf2 == pf1) =
                        if ((ps1 + ps2) /= 0)
                        then (pf1, (ps1 + ps2)) : ps
                        else  ps 
                  |otherwise = (pf1,ps1) : insert ps p2
      in foldl insert p1 p2

--todo Задача 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne 
unify as = add [] as

--todo Задача 2.a -----------------------------------------
findFree :: [PolinomOne] -> [Int]
findFree p = [ p1 | ((p1,p2):[]) <- p]

--todo Задача 2.b -----------------------------------------
iswfCommon ::  [PolinomOne]  -> Bool 
iswfCommon p = and([ all ((`elem` (0:findFree p)).fst) x | x <- p])

--todo Задача 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = null (filter (\row -> length row /= 1) le)

--todo Задача 3.b -----------------------------------------
solveSimple :: Linear -> Maybe [PolinomOne] 
solveSimple le = if (or ([ b /= 0 | (b:[]) <- le ]) == False)
                  then Just [] else Nothing

--todo Function 4.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow le = find le 1
        where find :: Linear -> Int -> Maybe Int
              find ln cn | null ln = Nothing
                             | null (head ln) = Nothing
                             | head (head ln) /= 0 = Just cn
                             | otherwise = find (tail ln) (cn + 1)

--todo Function 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow le i = le!!(i-1) : (drop 1 (takeOut (splitAt (i-1) le)))
                where takeOut (x,y) = x ++ [le!!(0)] ++ (drop 1 y)

--todo Function 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep fs rs = [ minus (drop 1 aji) ( map (*(aji!!0/fs!!0)) (drop 1 fs) ) | aji <- rs ]
                         where minus [] _ = []
                               minus (a:as) (b:bs) = (a-b) : minus as bs  

--todo Function 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep (f:[]) vs = [] : vs
reverseStep fs@(f:_) vs = unify (find (drop 1 (map (/ ((f)*(-1))) fs)) vs []): vs
              where find :: Row -> [PolinomOne] -> PolinomOne -> PolinomOne
                    find [] _ ac              = ac
                    find (cr:crw) [] ac       = ac ++ [(0, (cr*(-1)))]
                    find (cr:crw) (pl:crp) ac = find crw crp (ac ++ coef cr pl)

--todo Function 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne]
gauss i l = help l i
      where help :: Linear -> Int -> Maybe [PolinomOne]
            help l iter
                  | isSimple l = solveSimple l
                  | null l = Just []
                  | otherwise = case findRow l of
                         Just a -> case help (forwardStep (head (exchangeRow l a)) (tail (exchangeRow l a))) (iter + 1) of
                                    Just p ->  Just (reverseStep (head (exchangeRow l a)) p)
                                    _ -> Nothing
                         _ -> case help (rcmb 2 l) (iter + 1) of
                                    Just p -> Just ([(iter, 1)]:p)
                                    _ -> Nothing
            rcmb :: Int -> Linear -> Linear
            rcmb i l = [[q!!el|el <- [i-1..length q - 1]]| q <- l]

--todo Function 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool
testEquation pos rw = test pos rw [] (last rw)
                where test :: [PolinomOne] -> Row -> PolinomOne -> Rational -> Bool
                      test [] [] res finB = (length res == 1) && (fst (head res) == 0) && (snd (head res) == finB)
                      test poly [_] res finB = test poly [] (unify res) finB
                      test (pl:poly) (cf:row) res finB = test poly row (res ++ map (\el -> (fst el, snd el * cf)) pl) finB
                      test [] _ _ _ = False
                      test _ [] _ _ = False 

--todo Function 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool
testLinear pos le = null (filter (== False) (map (testEquation pos) le))

--todo Function 8 -----------------------------------------
solving :: Linear -> Solution
solving le = case gauss 1 ([0 % 1 | _ <- head le] : le) of
      Nothing -> Empty
      Just polLs -> if null (filter (not . null . filter (\pr -> fst pr /= 0)) polLs)
                    then One (map (snd . head) polLs)
                    else Many polLs

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty
sol2 = Empty
sol3 = Many res3
sol4 = One [62/15, -17/15 -4/3]