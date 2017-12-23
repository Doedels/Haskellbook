module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Show, Ord)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (t -> a) -> BinaryTree t -> BinaryTree a
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' = insert' 1(insert' 3(insert' 9(insert' 7(insert' 5 Leaf))))

mapExpected = Node (Node (Node Leaf 2 Leaf) 4 Leaf) 6 (Node Leaf 8 (Node Leaf 10 Leaf))

mapOkay = if mapTree (+1) testTree' == mapExpected
  then print "succes" else error "test failed!"

inorder :: BinaryTree a -> [a]
inorder bt = go bt []
  where go Leaf xs                = xs
        go (Node left a right) xs = go left xs ++ [a] ++ go right xs

reverseorder :: BinaryTree a -> [a]
reverseorder bt = go bt []
  where go Leaf xs                = xs
        go (Node left a right) xs = go right xs ++ [a] ++ go left xs

preorder :: BinaryTree a -> [a]
preorder bt = go bt []
  where go Leaf xs                = xs
        go (Node left a right) xs = [a] ++ go left xs ++ go right xs

postorder :: BinaryTree a -> [a]
postorder bt = go bt []
  where go Leaf xs                = xs
        go (Node left a right) xs = go left xs ++ go right xs ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
                then putStrLn "Preorder fine!"
                else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "postorder failed check"

main :: IO ()
main = do testPreorder; testInorder; testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z bt = foldr f z (inorder bt)
