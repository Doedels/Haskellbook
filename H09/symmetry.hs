module Symmetry where
-- 1
myWords :: String -> [String]
myWords string = go string []
  where go s xs
         | s == "" = reverse xs
         | otherwise = go (drop 1 (dropWhile (/= ' ') s))
                          ((takeWhile (/= ' ') s) : xs)

{- function words from the prelude
words s = case dropWhile {-partain:Char.-}isSpace s of
            "" -> []
            s' -> w : words s''
              where (w, s'') =
                break {-partain:Char.-}isSpace s'
-}
