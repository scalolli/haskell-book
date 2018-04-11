module BinaryTrees where

    data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Leaf
            deriving (Eq, Show, Ord)

    unfold :: (a -> Maybe (a, b, a))
           -> a
           -> BinaryTree b
    unfold f a =
            case (f a) of
                Just (x, b, y) -> Node (unfold f x) b (unfold f y)
                Nothing -> Leaf

    treeBuild :: Integer -> BinaryTree Integer
    treeBuild n = unfold (\a -> if(a > n) then Nothing else Just(a + 1, a, a + 1)) 0
