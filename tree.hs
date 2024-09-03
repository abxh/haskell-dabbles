data BinTree a
  = Leaf
  | Node a (BinTree a) (BinTree a)
  deriving (Show)

preorder :: BinTree a -> [a]
preorder Leaf = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r

inorder :: BinTree a -> [a]
inorder Leaf = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

postorder :: BinTree a -> [a]
postorder Leaf = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

inorderf :: (a -> b) -> BinTree a -> [b]
inorderf f Leaf = []
inorderf f (Node x l r) = inorderf f l ++ [f x] ++ inorderf f r

reflect :: BinTree a -> BinTree a
reflect Leaf = Leaf
reflect (Node x l r) = Node x (reflect r) (reflect l)

height :: BinTree a -> Integer
height Leaf = 0
height (Node x l r) = 1 + max (height l) (height r)

size :: BinTree a -> Integer
size Leaf = 0
size node = 1 + size l + size r
  where
    (Node x l r) = node

balanced :: BinTree a -> Bool
balanced Leaf = True
balanced node =
  height l == height r
    && balanced l
    && balanced r
  where
    (Node x l r) = node

data Exp
  = Const Integer
  | Add Exp Exp
  | Mult Exp Exp

eval :: Exp -> Integer
eval (Const x) = x
eval (Add x y) = eval x + eval y
eval (Mult x y) = eval x * eval y

linSearch :: Eq a => a -> [(a, b)] -> Maybe b
linSearch key [] = Nothing
linSearch key ((k, v) : xs)
  | key == k = Just v
  | otherwise = linSearch k xs

binSearch :: Ord a => a -> BinTree (a, b) -> Maybe b
binSearch key Leaf = Nothing
binSearch key (Node (k, v) l r)
  | key == k = Just v
  | key < k = binSearch key l
  | otherwise = binSearch key r

insert :: Ord a => (a, b) -> BinTree (a, b) -> BinTree (a, b)
insert (key, val) Leaf = Node (key, val) Leaf Leaf
insert (key, val) root
  | key < curr_key = Node (curr_key, curr_val) (insert (key, val) curr_left) curr_right
  | key > curr_key = Node (curr_key, curr_val) curr_left (insert (key, val) curr_right)
  | otherwise = Node (key, val) curr_left curr_right
  where
    (Node (curr_key, curr_val) curr_left curr_right) = root

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f Leaf = Leaf
mapTree f root = Node p' l' r'
  where
    (Node p l r) = root
    p' = f p
    l' = mapTree f l
    r' = mapTree f r

concatTree :: BinTree [a] -> [a]
concatTree Leaf = []
concatTree root = concatTree left ++ val ++ concatTree right
  where
    (Node val left right) = root

zipTree :: BinTree a -> BinTree b -> Maybe [(a, b)]
zipTree Leaf Leaf = Just []
zipTree Leaf _ = Nothing
zipTree _ Leaf = Nothing
zipTree b0 b1 =
  do
    list_l <- zipTree b0l b1l
    list_m <- Just [(b0v, b1v)]
    list_r <- zipTree b0r b1r
    Just (list_l ++ list_m ++ list_r)
  where
    (Node b0v b0l b0r) = b0
    (Node b1v b1l b1r) = b1

zipWithTree :: (a -> b -> c) -> BinTree a -> BinTree b -> Maybe [c]
zipWithTree f Leaf Leaf = Just []
zipWithTree f _ Leaf = Nothing
zipWithTree f Leaf _ = Nothing
zipWithTree f b0 b1 =
  do
    list_l <- zipWithTree f b0l b1l
    list_m <- Just [f b0v b1v]
    list_r <- zipWithTree f b0r b1r
    Just (list_l ++ list_m ++ list_r)
  where
    (Node b0v b0l b0r) = b0
    (Node b1v b1l b1r) = b1

inorderEfficient :: BinTree a -> [a]
inorderEfficient t = _inorder t []
  where
    _inorder Leaf acc = acc
    _inorder (Node x l r) acc = _inorder l (x : _inorder r acc)

appendTree :: BinTree a -> [a] -> [a]
appendTree Leaf acc = acc
appendTree (Node x l r) acc = appendTree l (x : appendTree r acc)
