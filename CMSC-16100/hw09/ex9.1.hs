data BinaryTree a
  = EmptyTree
  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show)
{-}
class Functor_ t where
  fmap :: (a -> b) -> t a -> t b
  -}

instance Functor BinaryTree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node a b c) = Node (f a) (fmap f b) (fmap f b)
