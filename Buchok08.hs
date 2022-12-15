{-# OPTIONS_GHC -Wall #-}
module HWC08 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

--todo Задача 1 -----------------------------------------
spaces :: String -> String
spaces = dropWhile isSpace

--todo Задача 2.a ----------------------------------------- 
manyT :: String -> (String,String)
manyT s = (takeWhile sT s, dropWhile sT s)

--todo Задача 2.b ----------------------------------------- 
value :: String -> (String,String)
value s = (takeWhile sV s, dropWhile sV s)

--todo Задача 2.c ----------------------------------------- 
manyN :: String -> (String,String)
manyN s = (takeWhile sN s, dropWhile sN s)

-- todo delete
-- <sT> ::= довільний символ, крім символів ‘<’ і ‘>’
-- <sV> ::= довільний символ крім символу  ‘”’
-- <sN> :: isDigit isLetter . -
sT, sV, sN :: Char -> Bool
sT x = x /= '<' && x /= '>'
sV x = x /= '"'
sN x = isDigit x || isLetter x || x == '.' || x == '-'

--todo Задача 3.a -----------------------------------------
name :: String ->  Maybe(String,String) 
name s =
  case manyN s of
    ("", _) -> Nothing
    (lex, str) -> Just (lex, str)

--todo Задача 3.b -----------------------------------------
text :: String ->  Maybe(String,String) 
text s =
  case manyT s of
    ("", _) -> Nothing
    (lex, str) -> Just (lex, str)

--todo Задача 3.c -----------------------------------------
fullValue :: String ->  Maybe(String,String) 
fullValue ('"':str) =
  case value str of
    (lex, ('"':r)) -> Just (lex, r) 
    _ -> Nothing

--todo Задача 4.a -----------------------------------------
attrib :: String -> Maybe ((String, String), String)
attrib str =
  case name (spaces str) of
    Just (_, "") -> Nothing
    Just (atn, rest) -> case value rest of    
        (eq, other) -> case spaces eq of     
            ('=':_) -> case fullValue other of
                Just (atv, r) -> Just ((atn, atv), r)
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

--todo Задача 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String)
manyAtt s = Just (listAttributes s)

listAttributes :: String -> (Attributes, String)
listAttributes s = case attrib s of
    Just (a, str) -> (a:attr, lefted)
     where (attr, lefted) = listAttributes str
    Nothing -> ([], s)

--todo Задача 5.a -----------------------------------------
begTag :: String -> Maybe ((String, Attributes), String)
begTag string = case spaces string of
    ('<':rest) -> case name rest of
        Just (nm, other) -> case spaces other of
            ('=':_) -> Nothing
            _ -> case manyAtt other of
                Just (attr, r) ->Just ((nm, attr), tail (spaces r))
                _ -> Nothing
        _ -> Nothing
    _ -> Nothing

--todo Задача 5.b -----------------------------------------
endTag :: String -> Maybe (String, String)
endTag string =
  case spaces string of
    ('<':'/':other) ->case name other of
        Just (nm, r) ->case spaces r of
            '>':rem -> Just (nm, rem)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

--todo Задача 6.a -----------------------------------------
element :: String -> Maybe (XML,String) 
element s = case begTag s of
            Just ((nm, attr), other) -> case manyXML other of
                                   Just (someXML, lefted) -> case endTag lefted of
                                                         Just(end, str) -> if nm == end then Just ((Element nm attr someXML), str)
                                                                           else Nothing
                                                         _ -> Nothing
                                   _ -> Nothing
            _ -> Nothing

--todo Задача 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml st = case element st of
         Just (xs, rst1) -> Just (xs, rst1)
         Nothing -> case text st of
                    Just (tx, rst2) -> Just (Text tx, rst2)
                    _ -> Nothing 

--todo Задача 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML s | null xmls = 
  if endTag st == Nothing then Nothing else Just (xmls, st)           
    | otherwise = case last xmls of
                  Text _  ->  if endTag st == Nothing then Nothing else Just (xmls, st)
                  _  -> Just (xmls, st)
    where (xmls, st) = fxml s

fxml :: String -> ([XML], String)
fxml s = case xml s of
            Just (x, lefted) -> (x:xmls, other)
             where (xmls, other) = fxml lefted
            _ -> ([], s)

--todo Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML s = case element (spaces s) of  
            Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
            Nothing -> Nothing  


-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



