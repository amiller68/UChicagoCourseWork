instance Applicative (Either a) where
  (<*>) :: Either a (b -> c) -> Either a b -> Either a c
  Left a <*> _ = Left a
  (<*>) = <$>

  pure :: b -> Either a -> Either a b
  pure b = Either a b
