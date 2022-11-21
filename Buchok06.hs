{-# OPTIONS_GHC -Wall #-}
module Buchok06 where

import Data.List

newtype Poly a = P [a]

--todo Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P [0,1]

--todo Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = 
        let h_drop = dropWhile (==0).reverse
        in  h_drop p1 == h_drop p2
        
 
--todo Задача 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
   show (P a)
    | filter (/=0) a == [] = show (0::Int)
    | otherwise = intercalate " + " . show_one . reverse $ zip [(0::Int)..] a
                where   show_one [] = []
                        show_one ((i,coef):xs)
                            | coef == 0 = show_one xs
                            | i == 0 && coef == 1 = "1" : show_one xs
                            | i == 0 && coef == -1 = "-1" : show_one xs
                            | i == 0 = h_c_show coef : show_one xs
                            | i == 1 = (h_c_show coef ++ "x") : show_one xs
                            | otherwise = (h_c_show coef ++ "x^" ++ show i) : show_one xs
                        h_c_show 0 = ""
                        h_c_show 1 = ""
                        h_c_show (-1) = "-"
                        h_c_show y = show y

--todo Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) =
            let add :: Num a => [a] -> [a] -> [a]
                add [] p2 = p2
                add p1 [] = p1
                add (p:p1) (pp:p2) = (p + pp) : add p1 p2 
            in P (add a b)
          
--todo Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = 
        let dgrs xs = zip [(0::Int)..] xs
            riseDgr xs deg
                  | deg == 0 = xs
                  | otherwise = riseDgr (0:xs) (deg-1)
            multOne (deg, coef) poly = P $ riseDgr (map (\xx -> xx*coef) poly) deg
        in foldl (+) (P [0]) (map (\xx -> multOne xx b) (dgrs a))

--todo Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P [-i|i <- a]
    fromInteger a = P [fromInteger a]
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

--todo Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P a) xx = sum $ map (\(pow,coeff) -> coeff * (xx^pow)) $ zip [(0::Int)..] a

--todo Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv n f = deriv (nderiv (n-1) f)

--todo Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P [])     = P []
    deriv (P (_:[])) = P []
    deriv (P (_:xs)) = x * deriv (P xs) + (P xs)

