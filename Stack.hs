

module Stack where

type (Stack a) = [a]

empty :: Stack a
empty = []

push :: a -> Stack a -> Stack a
push x l = x:l

pop :: Stack a -> (a, Stack a)
pop s =
    case s of
        []   -> error "empyt stack, nothing to pop"
        x:xs -> (x, xs)
