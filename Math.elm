module Math
( max
, min
) where

max : Number -> Number -> Number
max = compare (>)

min : Number -> Number -> Number
min = compare (<)

compare : (a -> a -> Bool) -> a -> a -> a
compare f x y = if f x y then x else y