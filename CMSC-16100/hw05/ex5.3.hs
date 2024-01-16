

-- | A module for handling binary trees which store values at their internal nodes

-- module BinaryTree where

data BinaryTree a    = EmptyTree  | Node a (BinaryTree a) (BinaryTree a)
{-}
instance Foldable BinaryTree where
  foldr combiner base tree = foldMap combiner tree base where
      foldMap _ EmptyTree rest = rest
      foldMap combiner (Node a left right) rest = (foldMap combiner left (combiner a (foldMap combiner right rest)))
-}
instance Foldable BinaryTree where
  foldr = foldMap where
    foldMap _ rest EmptyTree = rest
    foldMap combiner rest (Node a left right)= (foldMap combiner (combiner a $ foldMap combiner rest right) left)

test :: BinaryTree Integer
test = Node 3
      (Node 1
        (Node 0 EmptyTree EmptyTree)
        (Node 2 EmptyTree EmptyTree))
      (Node 5
        (Node 4 EmptyTree EmptyTree)
        (Node 6 EmptyTree EmptyTree))

flip23 a b c = a c b

{-
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)

visit :: BinaryTree a -> [a]
visit EmptyTree = []
visit (Node a left right) = visit left ++ [a] ++ visit right

test :: BinaryTree Integer
test = Node 3
        (Node 1
          (Node 0 EmptyTree EmptyTree)
          (Node 2 EmptyTree EmptyTree))
        (Node 5
          (Node 4 EmptyTree EmptyTree)
          (Node 6 EmptyTree EmptyTree))

foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
foldr combiner base tree = foldMap combiner tree base

foldMap :: (a -> b -> b) -> BinaryTree a -> b -> b
foldMap _ EmptyTree = id
--foldMap combiner (Node a left right) rest =
  --(foldMap combiner left (combiner a (foldMap combiner right rest)))
  --(foldMap combiner left (combiner a $ foldMap combiner right rest))
  --(foldMap combiner left (combiner a $ foldMap combiner right rest))
foldMap combiner (Node a left right) =
  foldMap combiner left . combiner a . foldMap combiner right

-}
