-- building functions opdracht 2
-- a
addAcclemation :: String -> String
addAcclemation word = word ++ "!"

-- b
getLetter5AsString :: String -> String
getLetter5AsString word = take 1(drop 4 word)

-- c
drop9 :: String -> String
drop9 word = drop 9 word

-- opdracht 3
thirdLetter :: [a] -> a
thirdLetter word = word !! 2

-- odracht 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! (x - 1)
