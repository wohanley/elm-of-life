module ListUtil where

import Batteries as Batt

range : Int -> Int -> [Int]
range from to = tailRange from to []

tailRange : Int -> Int -> [Int] -> [Int]
tailRange from to list =
    if from == to
    then list
    else from :: (tailRange (from + 1) to list)

get : Int -> [a] -> a
get index xs =
    if index == 0
    then head xs
    else get (index - 1) (tail xs)

getAll : [Int] -> [a] -> [a]
getAll indices xs = map ((Batt.flip get) xs) indices

flatten : [[a]] -> [a]
flatten lists = foldl (++) [] lists

indexedMap : (Int -> a -> b) -> [a] -> [b]
indexedMap f xs = tailIndexedMap f 0 xs

tailIndexedMap : (Int -> a -> b) -> Int -> [a] -> [b]
tailIndexedMap f fromIndex xs =
    case xs of
      [] -> []
      head::tail -> (f fromIndex head) ::
            (tailIndexedMap f (fromIndex + 1) tail)