{-# OPTIONS_GHC -Wall #-}
module Buchok01 where

--todo Задача 1 -----------------------------------------
--factorial calculation function
factorial n = if (n>=0) then calculateF(n) else 0
factorial :: Integer -> Integer

--recursive factorial calculation help function
calculateF :: Integer -> Integer
calculateF n = if(n==1) then 1 else n * calculateF(n-1)

--todo Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum as bs 
    |null as = bs
    |null bs = as
    |length as < length bs = calculateListS (fill as ((length bs) - (length as))) bs
    |length bs < length as = calculateListS as (fill bs ((length as) - (length bs)))
    |otherwise = calculateListS as bs

--recursive adding two lists 
calculateListS :: [Int] -> [Int] -> [Int] 
calculateListS as bs = if(null as || null bs) 
    then []
    else (head as + head bs) : calculateListS (tail as) (tail bs) 

--filling array with n zeroes, where n = length
fill :: [Int] -> Int -> [Int] 
fill as myLength = if myLength == 0 then [] else as ++ zerosArray myLength
 
 --creating array full of zeros with length = length
zerosArray :: Int -> [Int] 
zerosArray myLength = if myLength == 0 then [] else 0 : zerosArray(negate myLength) 

--todo Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven as =
     if (length as <= 1)
     then as
     else concTwoValRevArr as 2 (length as)

--concatination of reversed two value arrays
concTwoValRevArr :: [Int] -> Int -> Int -> [Int] 
concTwoValRevArr as iterator myLength =
    if((iterator - myLength) > 1)
    then [] 
    else (getTwoValArr (as) (iterator) (iterator-2)) ++ (concTwoValRevArr as (iterator+2) (myLength)) 

--getting two value arrays out of initial array and reverse it
getTwoValArr :: [Int] -> Int -> Int -> [Int] 
getTwoValArr as myLength iterator =
     if (myLength==iterator)
     then [] 
     else (getTwoValArr as myLength (iterator+1)) ++
      if null (drop iterator as)
         then [] 
         else [(head (drop iterator as))]

--todo Задача 4 -----------------------------------------
position :: Int -> [Int] -> Int
position n as =
    if null as then -1 else (length as) - (findPos (tail as) (length as) n (head as))

--recursive taking element by element from as where n' is the element
findPos :: [Int] -> Int -> Int -> Int -> Int
findPos as myLength n n' 
    |n' == n = (length as) +1
    |null as = myLength + 1
    |otherwise = findPos (tail as) myLength n (head as)
         
--todo Задача 5 -----------------------------------------
--getting array of unigue values out of any array
set :: [Int] -> [Int] 
set [] = []
set as = uniqSet as []

--putting unique values in new array 
uniqSet :: [Int] -> [Int] -> [Int] 
uniqSet as bs =
     if null as 
        then bs
        else 
            if elem (head as) bs 
                then uniqSet (tail as) bs 
                else uniqSet (tail as) (bs ++ [(head as)])
    
--todo Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union [] bs = set bs
union as [] = set as
union as bs = calcUnion as bs []

--recursive creating union array
calcUnion :: [Int] -> [Int] -> [Int] -> [Int]
calcUnion as bs cs
    | null as = reverse cs ++ reverse (addOne bs cs [])
    | null bs = reverse cs ++ reverse (addOne as cs [])
    | otherwise = calcUnion (tail as) (tail bs) (addTwo (head as) (head bs) cs)

--recursive creating array keeping uniquness of numbers with another array
addOne :: [Int] -> [Int] -> [Int] -> [Int]
addOne as bs cs = 
    if null as 
        then cs
        else 
            if (elem (head as) bs) 
                then addOne (tail as) bs cs 
                else addOne (tail as) bs ((head as) : cs)

--recursive trying adding two num a and b to two value array    
addTwo :: Int -> Int -> [Int] -> [Int]
addTwo a b cs =
     if(a==b)
         then (if (elem a cs) then cs else (a:cs))
         else (if (elem b cs) then addTwo a a cs else addTwo a a (b:cs))

--todo Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection as bs 
    |null as   = bs
    |null bs   = as
    |otherwise = reverse (formArr as bs [])

--forming intersection
formArr :: [Int] -> [Int] -> [Int] -> [Int]
formArr as bs cs 
    | null as = cs
    | not(elem (head as) cs) && (elem (head as) bs) = formArr (tail as) bs ((head as):cs)
    | otherwise = formArr (tail as) bs cs

--todo Задача 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = tail(scanl (*) 1 [1..])

