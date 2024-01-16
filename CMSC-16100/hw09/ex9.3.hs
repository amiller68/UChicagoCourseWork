{-}
class Functor_ t where
  fmap_ :: (a -> b) -> t a -> t b-}

instance Functor ((->) a) where
fmap :: (b -> c) -> (x -> b) -> x -> c
fmap = (.)

-- fmap :: ((a -> b) -> c) -> (a -> b) -> (a -> c)
