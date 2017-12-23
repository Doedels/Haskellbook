module Exercises where
-- 1
stops = "pbtdkg"
vowels = "aeiou"
-- a
svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
-- b
svsp = [('p', y, z) | y <- vowels, z <- stops]
-- c
noun1 = ["child", "company", "day", "eye", "fact" ,"government"]
verb = [" wanted ", " worked ", " asked ", " needed "]
noun2 = ["problem", "right", "room", "story", "student"]

sentence = [ x ++ y ++ z | x <- noun1, y <- verb, z <- noun2]
-- 2
sf :: String -> Int
sf x = div (sum (map length (words x))) (length (words x))
-- 3
sf1 :: Fractional a => String -> a
sf1 x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

one = sf "a man and his dog" -- 2
two = sf1 "a man and his dog" -- 2.6
