import Data.Traversable

data BinaryTree a
        = Empty
        | Node (BinaryTree a) a (BinaryTree a)
        deriving Show

testTree =
            Node (Node Empty
                       1
                       (Node Empty 2 Empty))
                  3
                  (Node Empty
                        4
                        (Node Empty 5 Empty))

instance Functor BinaryTree where
        fmap _ Empty = Empty
        fmap f (Node left a right) =
            Node (fmap f left) (f a) (fmap f right)

instance Foldable BinaryTree where
  foldr f acc Empty = acc
  foldr f acc (Node left a right) =
    foldr f (f a (foldr f acc right)) left


instance Traversable BinaryTree where
        traverse _ Empty = pure Empty
        traverse f (Node left a right) =
             Node <$> traverse f left <*> f a <*> traverse f right

addLabels :: (Traversable t) => t a ->  t (Int,a)
addLabels ta = snd $ mapAccumL (\acc node -> (acc+1, (acc, node))) 1 ta
