data Pair a b = Pair a b

instance (Eq a, Eq b) => Eq (Pair a b) where
  Pair a1 b1 == Pair a2 b2 = a1 == a2 && b1 == b2

instance (Ord a, Ord b) => Ord (Pair a b) where
  (Pair a1 b1) <= (Pair a2 b2) = (a1 <= a2)

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair a b) = show ((,) a b)
