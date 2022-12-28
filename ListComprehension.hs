
module ListComprehension where

import Prelude

myLength :: [a] -> Int
myLength l = sum [ 1 | _ <- l ]

myFirsts :: [(a, b)] -> [a]
myFirsts l = [ x | (x, _) <- l ]

myConcat :: [[a]] -> [a]
myConcat l = [ x | xs <- l, x <- xs ]

myFactors :: Int -> [Int]
myFactors n = [ x | x <- [1..n], mod n x == 0 ]

myPrimes :: Int -> [Int]
myPrimes n = [ x | x <- [1..n], myFactors x == [1,x] ]