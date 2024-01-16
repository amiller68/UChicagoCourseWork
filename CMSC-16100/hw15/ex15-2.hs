newtype State s a = State { runState :: s -> (a, s)}

instance Functor (State s) where
        fmap f ma = State $ (\(a,s) -> (f a,s)) . runState ma

instance Applicative (State s) where
  --pure a = State $ \s -> (a,s)
  pure a = State $ (,) a
  --(,) a :: b -> (a,b)
  --(<*>) :: State s a -> (a -> State s b) -> State s b
  --(<*>) :: (\s -> (a,s)) -> (a -> (\s -> (b,s))) ->
  af <*> aa = State $ (\(f, t) (a, u) -> (f a, u)) . runState as . runState af
