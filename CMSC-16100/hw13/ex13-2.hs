newtype First a = First { getFirst :: Maybe a} deriving (Show)
newtype Last a = Last { getLast :: Maybe a} deriving (Show)

instance Semigroup (First a) where
  First a <> _ = First a
  Nothing <> First a = First a
instance Monoid (First a) where
  mempty = Nothing

instance Semigroup (Last a) where
  _ <> Last a = Last a
  Last a <> Nothing = Last a

instance Monoid (Last a) where
  mempty = Nothing
