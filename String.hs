
module String where

import Prelude

myLinesH :: String -> String -> [String]
myLinesH s acc =
    case s of
        []   -> [acc]
        x:xs -> if x /= '\n' then myLinesH xs (acc++[x]) 
                else acc : myLinesH xs ""

myLines :: String -> [String]
myLines s = myLinesH s ""

myUnlines :: [String] -> String
myUnlines l =
    case l of
        []   -> ""
        x:xs -> x ++ "\n" ++ myUnlines xs


myWordsH :: String -> String -> [String]
myWordsH s acc =
    case s of
        []   -> [acc]
        x:xs -> if x /= ' ' then myWordsH xs (acc++[x]) 
                else acc : myWordsH xs ""

myWords :: String -> [String]
myWords s = myWordsH s ""


myUnwords :: [String] -> String
myUnwords l =
    case l of
        []   -> ""
        [x]  -> x
        x:xs -> x ++ " " ++ myUnwords xs


