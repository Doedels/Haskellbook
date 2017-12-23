module Exercises where

import           Data.Char
import           Data.List

-- as-paterns 1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf xs ys = go xs ys
  where go [] _             = True
        go _ []             = False
        go aa@(a:as) (b:bs) = if a == b then go as bs else go aa bs
-- 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map (\x -> case x of zs@(y:ys) -> (zs, toUpper y : ys)) (words xs)

-- Language exercises 1
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
-- 2
findDots :: String -> [Int]
findDots xs = reverse (filter (< length xs - 2) (elemIndices '.' xs))

splitSentence :: String -> [String]
splitSentence xs = go (findDots xs) [xs]
  where go [] ys         = ys
        go (z:zs) (a:as) = go zs ( case splitAt (z + 2) a of (v,w) -> [v, w] ++ as )

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capitalizeWord (splitSentence xs)
-- Phone Exercise

-- to do (mayby someday)

-- Hutton's Razor
data Expr = Lit Integer | Add Expr Expr deriving (Show)
-- 1
eval :: Expr -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
-- 2
printExpr :: Expr -> String
printExpr (Lit x)   = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
