{-# OPTIONS_GHC -Wall #-}
module HWC04 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int  deriving (Show, Eq)
type Program = [Command]
type ConfigC = (Int, Int, [Int])

--todo Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (a:as) (b:bs)
    | a == b = isPrefix as bs
    |  True  = False

--todo Задача 2 ------------------------------------ 
substitute :: Substitution -> Int -> String -> String
substitute (s1,s2,_) i w
                | isPrefix' = take i w ++ s2 ++ drop (length s1 + i) w
                |   True    = w
                    where
                         isPrefix' = isPrefix s1 (drop i w)

--todo Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition w sub = find w sub 0
        where
             find::String -> Substitution -> Int -> [(Substitution,Int)]
             find w sub@(s1,_,_) acc
                        | null w && null s1 = [(sub, acc)]
                        | null w = []
                        | isPrefix s1 w = (sub, acc) : find (tail w) sub (acc+1)
                        | True          = find (tail w) sub (acc+1)

--todo Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w = concatMap (findPosition w) algo

--todo Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo cfg@(_,_,str) = step (head (take 1 (findAll algo str))) cfg
    where
        step :: (Substitution, Int) -> ConfigA -> ConfigA
        step ((sub@(_,_,s3)), idx) (b,acc,str) = ((not s3), (acc+1), (substitute sub idx str))
 
--todo Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word = check (until cond step (True, 0, word)) 
                            where
                                cond :: ConfigA -> Bool
                                cond (b,i,s) = ((not b) || (i>m))
                                step :: ConfigA -> ConfigA
                                step (b,i,s) = stepA algo (b, i, s)
                                check :: ConfigA -> Maybe String
                                check (b,i,s)
                                    |b    = Nothing
                                    |otherwise = Just s

--todo Задача 7 ------------------------------------
maximReg :: Program -> Int 
maximReg pr = maximum ([takeCmd n | n <- pr])
                where 
                    takeCmd :: Command -> Int
                    takeCmd (Z a) = a
                    takeCmd (S a) = a
                    takeCmd (T a b) = max a b
                    takeCmd (J a b c) = max a b

--todo Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int]
ini pr ir = gen pr ir (maximReg pr - length ir)
         where
            gen:: Program -> [Int] -> Int -> [Int]
            gen pr ir acc 
                        | acc <= 0 =  ir
                        | True = gen pr (ir ++ [0]) (acc - 1)

upd :: [Int] -> Int -> Int-> [Int]
upd [] _ _ = []
upd (_:reg) 0 v = v:reg
upd (r:reg) (n) v
            | n < 0 = r:reg
            | True  = r: upd reg (n-1) v


--todo Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC pr cfg@(nm, st, rg)
                | nm <= (length pr) = execCmd (pr!!(nm-1)) cfg 
                | True              =  (nm, st+1, rg)
                    where 
                        execCmd :: Command -> ConfigC -> ConfigC
                        execCmd (Z idx) (nxtC,cmdN,regs) = (nxtC+1, cmdN+1, upd regs (idx-1) 0)
                        execCmd (S idx) (nxtC,cmdN,regs) = (nxtC+1, cmdN+1, upd regs (idx-1) (regs!!(idx-1) + 1))
                        execCmd (T idxFrom idxTo) (nxtC,cmdN,regs) = (nxtC+1, cmdN+1, upd regs (idxTo-1) (regs!!(idxFrom-1)))
                        execCmd (J r1 r2 cmdIdx) (nxtC,cmdN,regs)
                                        | (regs!!(r1-1)) /= (regs!!(r2-1)) = (nxtC+1, cmdN+1, regs)
                                        | True = (cmdIdx, cmdN+1, regs)


--todo Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int
evalC pr mx ir = check (until cond step (1, 0, (ini pr ir))) 
                            where
                                cond :: ConfigC -> Bool
                                cond (nm,st,_) = (nm > length pr) || (st > mx)
                                step :: ConfigC -> ConfigC
                                step (nm,st,rg) = stepC pr (nm, st, rg)
                                check :: ConfigC -> Maybe Int
                                check (_,st,rg)
                                    | st > mx  = Nothing
                                    | True = Just (head rg)


---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
