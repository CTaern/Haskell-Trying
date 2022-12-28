{-# LANGUAGE GADTs #-}

module Calendar where

import Prelude
import Aux
import Month
import Picture

pic :: Int -> Int -> String
pic t n = if 1 <= n && n <= t then show n else ""

daysOfMonth :: Month -> Year -> [Picture]
daysOfMonth m y =
    let (d, t) = monthInfo m y
    in myMap row (myMap (myRjustify 3) (myMap (pic t) [1-d..42-d]))

month :: Month -> Year -> Picture
month m y = tile (myGroupsOfSize 7 (daysOfMonth m y))

printMonth :: Month -> Year -> IO()
printMonth m y = 
    let weekdays = row " Su Mo Tu We Th Fr Sa"
    in printPic (above weekdays (month m y))

prepareMonth :: Month -> Year -> Picture
prepareMonth m y = 
    let weekdays = row " Su Mo Tu We Th Fr Sa"
    in above weekdays (month m y)

printYear :: Year -> IO()
printYear y =
    let l1 = [1..4]
        l2 = [5..8]
        l3 = [9..12]
        in printPic (tile ([recurse l1 y, recurse l2 y, recurse l3 y]))
        where
            recurse l y =
                case l of
                    []   -> []
                    x:xs -> (beside (prepareMonth x y) (blank 7 3)) : recurse xs y

