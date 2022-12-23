{-# OPTIONS_GHC -Wall #-}
module Buchok00 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

type Graph  = [[Int]]

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                  deriving (Show, Eq) 
--todo Задача 1 -----------------------------------------
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = equals : group others
    where
        (equals, others) = span (== x) (x:xs)
   
--todo Задача 2 -----------------------------------------
bagSubbag :: String -> String -> Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (a:b) res
             | elem a res = bagSubbag b (m_delete res a)
             | otherwise = False
--todo Задача 3 -----------------------------------------
bagUnion :: String -> String -> String
bagUnion [] st2 = st2
bagUnion (s:st1) st2 =
                    let amt_st1 = (occur_count s st1) + 1
                        amt_st2 = occur_count s st2
                        st1r = delete_all st1 s
                        st2r = delete_all st2 s
                        res = if amt_st1 > amt_st2
                                then [s | _<-[1..amt_st1]]
                              else [s | _<-[1..amt_st2]]
                              
                    in res ++ bagUnion st1r st2r
--todo Задача 4 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency a = [(x, occur_count x a) | x <- unique a]
--todo Задача 5 -----------------------------------------
instance Ord AbstractInteger where
    (<=) (Succ x) (Succ y) = x <= y
    (<=) (Pred x) (Pred y) = x <= y
    (<=) Zero Zero = True
    (<=) (Pred _) Zero = True
    (<=) (Succ _) Zero = False
    (<=) (Pred _) (Succ _) = True
    (<=) (Succ _) (Pred _) = False
    (<=) Zero (Pred _) = False
    (<=) Zero (Succ _) = True
   
--todo Задача 6 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger (Succ ai) = 1 + aiToInteger ai 
aiToInteger (Pred ai) = (-1) + aiToInteger ai
aiToInteger Zero = 0
 
--todo Задача 7 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs ai1 ai2 = absrt_plus ai1 ai2 Zero

absrt_plus :: AbstractInteger-> AbstractInteger -> AbstractInteger -> AbstractInteger
absrt_plus (Pred a1) (Succ a2) res = absrt_plus a1 a2 res
absrt_plus (Succ a1) (Pred a2) res = absrt_plus a1 a2 res
absrt_plus (Pred a1) (Pred a2) res = absrt_plus a1 a2 (Pred(Pred res))
absrt_plus (Succ a1) (Succ a2) res = absrt_plus a1 a2 (Succ(Succ res))

absrt_plus Zero Zero res           = res
absrt_plus a1 Zero res             = remain_plus a1 res
absrt_plus Zero a2 res             = remain_plus a2 res

remain_plus :: AbstractInteger -> AbstractInteger -> AbstractInteger
remain_plus (Succ a) res = remain_plus a (Succ res)
remain_plus (Pred a) res = remain_plus a (Pred res)
remain_plus Zero res     = res

--todo Задача 8 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs ai1 ai2 = fromInteger(aiToInteger ai1 * aiToInteger ai2)

--todo Задача 9 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs

    negate (Pred a) = Succ(negate a)
    negate (Succ a) = Pred(negate a)
    negate Zero = Zero

    fromInteger a
       | a > 0 = Succ(fromInteger (a-1))
       | a < 0 = Pred(fromInteger (a+1))
       | otherwise = Zero

    abs ai = 
        if aiToInteger ai > 0
            then ai
        else negate ai

    signum ai
       | aiToInteger ai > 0 = 1
       | aiToInteger ai < 0 = -1
       | otherwise = 0

--todo Задача 10 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay [] _ _ = []
shortWay gr a b =
     if res == [] then [] else res!!0
        where res = way gr b [[a]] 

way :: Graph -> Int -> [[Int]] -> [[Int]]
way _ _ [] = []
way gr a b = if length w > 0 then w else way gr a (help gr b)
        where w = [n | n <- b, n!!(length n-1)==a]
     
help :: Graph -> [[Int]] -> [[Int]]
help _ [] = []
help g (a:as) = [a++[x] | x <-adj, not (elem x a)] ++ help g as
            where adj = g!!(a!!(length a - 1))

--todo Задача 11 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting g 
        | length (components g) == 1 = True 
        | True = False

--todo Задача 12 ------------------------------------
components :: Graph -> [[Int]] 
components gr = comh gr 0 []

comh :: Graph -> Int -> [[Int]] -> [[Int]]
comh gr t r 
        | t == length gr = r
        | res = comh gr (t+1) r| otherwise = comh gr (t+1) $ r ++ [h]
            where 
                res = True `elem` [t `elem` x | x <- r]
                h = [x | x <- [t..length gr - 1], hw gr t x]

hw :: Graph -> Int -> Int -> Bool
hw gr a b = [] /= shortWay gr a b

--todo Задача 13 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity g e = maximum [length (shortWay g e res) | res <- heln g] - 1

--todo Задача 14 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter = maximum . find 

findRadius :: Graph -> Int 
findRadius = minimum . find

find :: Graph -> [Int]
find gr = [eccentricity gr a | a <- heln gr]

heln :: Graph -> [Int]
heln grap = [0..(length grap - 1)]

--todo Задача 15 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter g = [x | x <- [0..length g - 1], eccentricity g x == res ]
                where res = findRadius g

--todo Задача 16 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM a b c d) = b > 0 && maybe False (a>) (helpNode c) && maybe False (a<) (helpNode d) 

helpNode :: (Ord a) => BinTreeM a -> Maybe a 
helpNode (NodeM a _ _ _) = Just a
helpNode EmptyM = Nothing 

--todo Задача 17 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch r t = case r of
    NodeM a _ c d -> 
                    if a == t then True 
                               else 
                                if a < t then elemSearch d t 
                                else elemSearch c t
    EmptyM -> False

--todo Задача 18 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM a k l r) x =
    if x < a
        then NodeM a k(insSearch l x) r
        else if x > a
            then NodeM a k l (insSearch r x)
            else NodeM a (k+1) l r

--todo Задача 19 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _=  EmptyM
delSearch (NodeM a b c d) v | v == a = if b>1 
  then NodeM a (b-1) c d 
  else delhelp (NodeM a b c d )| v < a = NodeM a b (delSearch c v) d| otherwise = NodeM a b c (delSearch d v)

delhelp :: (Ord a) => BinTreeM a -> BinTreeM a
delhelp (NodeM _ _ l  EmptyM) = l
delhelp (NodeM _ _ EmptyM  d) = d
delhelp (NodeM _ b c d) = NodeM (leftEl d) b c d

leftEl :: (Ord a) => BinTreeM a -> a
leftEl (NodeM a _ EmptyM  _) = a
leftEl (NodeM _ _ c _) = leftEl c

--todo Задача 20 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = helpsort $ foldl insSearch EmptyM xs

helpsort::(Ord a)=> BinTreeM a -> [a]
helpsort EmptyM = []
helpsort (NodeM a b c d ) = helpsort c ++(replicate b a)++helpsort d

---------------------Тестові дані - Графи -------
gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

--------------------- Допоміжні функції -------
--видаляє символ з рядка, який трапиться перший зліва в рядку
m_delete :: String -> Char -> String
m_delete [] _ = ""
m_delete (a:b) res
            | a == res = b
            | otherwise = [a] ++ m_delete b res

--підраховує кількість елемента в масиві
occur_count :: Eq a => a -> [a] -> Int
occur_count x = length . filter (x==)

--видаляєм всі елементи з рядка, які ==a
delete_all :: String -> Char -> String
delete_all [] _ = []
delete_all (x:ss) a 
        | x==a = delete_all ss a
        | otherwise = [x]++(delete_all ss a)

-- робимо масив з унікальними елементами
unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = x : filter (/=x) (unique xs)