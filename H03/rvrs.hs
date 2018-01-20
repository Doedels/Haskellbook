module Reverse where

rvrs :: String -> String
rvrs word = drop 9 word ++ take 4 (drop 5 word) ++ take 5 word

main :: IO ()
main = print (rvrs "Curry is awesome")
