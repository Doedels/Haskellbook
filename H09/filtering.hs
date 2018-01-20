module Filtering where

multiOf3 = filter (\x -> rem x 3 == 0) [1..30]

m3l = length . filter (\x -> rem x 3 == 0) $ [1..30]

myFilter string = filter (\x -> x `notElem` ["and", "an", "a"]) $ words string
