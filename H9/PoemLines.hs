module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
             \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines string = go string []
  where go s xs
         | s == "" = reverse xs
         | otherwise = go (drop 1 (dropWhile (/= '\n') s))
                          ((takeWhile (/= '\n') s) : xs)

shouldEqual =
  ["Tyger Tyger, burning bright",
  "In the forests of the night",
  "What immortal hand or eye",
  "Could frame thy fearful symmetry?"]

main :: IO ()
main = print $ "are they equal? "
  ++ show (myLines sentences == shouldEqual)
-- 3
splitting :: Char -> String -> [String]
splitting with string = go string []
  where go s xs
         | s == "" = reverse xs
         | otherwise = go (drop 1 (dropWhile (/= with) s))
                          ((takeWhile (/= with) s) : xs)

{-
The Tyger (1794) BY WILLIAM BLAKE

Tyger Tyger, burning bright,
In the forests of the night;
What immortal hand or eye,
Could frame thy fearful symmetry?

In what distant deeps or skies.
Burnt the fire of thine eyes?
On what wings dare he aspire?
What the hand, dare seize the fire?

And what shoulder, & what art,
Could twist the sinews of thy heart?
And when thy heart began to beat,
What dread hand? & what dread feet?

What the hammer? what the chain,
In what furnace was thy brain?
What the anvil? what dread grasp,
Dare its deadly terrors clasp!

When the stars threw down their spears
And water'd heaven with their tears:
Did he smile his work to see?
Did he who made the Lamb make thee?

Tyger Tyger burning bright,
In the forests of the night:
What immortal hand or eye,
Dare frame thy fearful symmetry?
-}
