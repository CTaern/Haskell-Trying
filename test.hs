
import Prelude

myRange :: Int -> Int -> [Int]
myRange n m =
    if n > m then []
    else n : myRange (n+1) m

mySum :: [Int] -> Int
mySum l =
    case l of
        [] -> 0
        x:xs -> x + mySum xs

