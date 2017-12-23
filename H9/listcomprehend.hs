module ListComprehend where

tla = [x | x <- "Three Letter Acronym", elem x ['A'..'Z']]

acro xs = [x | x <- xs, x `elem` ['A'..'Z']]

myString xs = [x | x <- xs, x `notElem` "aeiouy"]

-- Square Cube
mySqr = [x ^ 2 | x <- [1..5]]
myCube = [y ^ 3 | y <- [1..5]]
-- 1
a = [(x, y) | x <- mySqr, y <- myCube]
b = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
c = length b

mySum xs = go xs 0
  where go [] s     = s
        go (y:ys) s = go ys $! (s + y)

myScanSum xs = go xs []
  where go [] sumList     = sumList
        go (y:ys) []      = go ys (y:[])
        go (y:ys) sumList = go ys ((y + head sumList):sumList)
