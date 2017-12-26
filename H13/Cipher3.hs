module Cipher3 where

import           Data.Char
import           Data.List
import           System.IO

normal :: [Char]
normal = ['a'..'z']

cipher :: (Ord a1, Num a1) => a1 -> [a] -> [a]
cipher shift xs
     | shift == 0 = xs
     | shift > 0  = cipher (shift - 1) (tail xs ++ [head xs])
     | shift < 0  = cipher (shift + 1) (last xs : init xs)

index :: Eq a => a -> [a] -> Int
index ch xs = case findIndex (== ch) xs of
  Just x  -> x
  Nothing -> -1

ceasar :: (Num t, Ord t) => t -> [Char] -> [Char]
ceasar shift string = map encode string
  where
    code = cipher shift normal
    encode x = if ind == (-1) then ' ' else code !! ind
      where
        ind = index (toLower x) normal

unCeasar :: (Ord t, Num t) => t -> [Char] -> [Char]
unCeasar shift string = ceasar (negate shift) string

-- Vigenère cipher
vig :: String -> String -> String
vig m k = go m k [] -- m = message, k = keyword
  where go [] _ zs = reverse . concat $ zs
        go xs [] zs = go xs k zs
        go (x:xs) (y:ys) zs = go xs ys ((ceasar (index (toLower y) normal) [x]) : zs)

unVig :: String -> String -> String
unVig m k = go m k []
  where go [] _ zs = reverse . concat $ zs
        go xs [] zs = go xs k zs
        go (x:xs) (y:ys) zs = go xs ys ((unCeasar (index (toLower y) normal) [x]) : zs)

stringOrInt :: [Char] -> Either Int [Char]
stringOrInt [] = Right "Something went wrong"
stringOrInt xs
       | all isDigit xs = Left $ makeInt (reverse xs) 1
       | otherwise = Right xs
         where makeInt [] _ = 0
               makeInt (y:ys) n = (digitToInt y * n) + makeInt ys (n * 10)

coding :: [Char] -> [Char] -> Either Int String -> [Char]
coding text "e" (Left int) = ceasar int text
coding text "d" (Left int) = unCeasar int text
coding text "e" (Right str) = vig text str
coding text "d" (Right str) = unVig text str
coding _ _ _ = "something went wrong"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please give the text: "
  text <- getLine
  putStr "(e)ncode or (d)ecode? "
  ed <- getLine
  putStr "Give a codeword for Vigenère, number for Ceasar: "
  code <- getLine
  putStrLn (coding text ed (stringOrInt code))
