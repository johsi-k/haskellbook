-- Higher-kinded Datatypes

-- identical to (a, b, c, d)
data Silly a b c d =
  MkSilly a b c d deriving Show

-- Silly   ::  * -> * -> * -> * -> *
-- Silly Int     :: * -> * -> * -> *
-- Silly Int String   :: * -> * -> *
-- Silly Int String Bool   :: * -> *
-- Silly Int String Bool String :: *


-- (,,,) :: * -> * -> * -> * -> *
-- (Int, String, Bool, String) :: *

-- Lists are polymorphic

-- as infix
-- data [] a = [] | a : []
-- any operator that starts with a (:) must be an infix type or data constructor
-- the type constructor -> is the only infix type constructor that doesn't begin with a colon

-- without infix
data List a = Nil | Cons a (List a)

nil :: List a
nil = Nil

listoNums :: List Integer
listoNums = Cons 3 (Cons 2 Nil)

-- How lists are kinded
-- List :: * -> *
-- [] :: * -> *

-- List Int :: *
-- [Int] :: *

-- Binary Tree

data BinaryTree a =
    Leaf -- terminal node
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
            => a
            -> BinaryTree a
            -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) -- Left and Right are BinaryTrees
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  -- | b > a  = Node left a (insert' b right)
  | otherwise = Node left a (insert' b right)

t1 = insert' 0 Leaf
-- t1 = Node Leaf 0 Leaf

t2 = insert' 3 t1
-- t2 = Node Leaf 0 (Node Leaf 3 Leaf)

t3 = insert' 5 t2
-- t3 = Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf))


-- Given the definition of BinaryTree above, write a map function for the data structure. The structure inherent in the definition of the type is all you need.

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  -- f can only be applied to a value of type a, not a BinaryTree
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


-- Convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = inorder left ++ inorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."


-- Write foldr for BinaryTree

-- any traversal order is fine
foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) =
  -- f can only be applied to values of type a and b, not a tree
  f a (foldTree f (foldTree f b left) right)

testFoldTree :: IO ()
testFoldTree =
  if foldTree (+) 0 testTree == 6
  then putStrLn "FoldTree fine!"
  else putStrLn "Bad news bears."
