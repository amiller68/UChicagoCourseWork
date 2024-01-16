newtype State s a = State { runState :: s -> (a, s)}

instance Functor (State s) where
--    fmap f ma = State $ \s ->
  --    let (a,t) = runState ma s
    --  in (f a,t)
  --  fmap f ma = State $ (\(a,s) -> (f a,s)) . runState ma
  --  fmap f ma = State $ (\(a,s) -> (f a, s)) . (\s -> (a,s))
    fmap f ma = State $ \s -> (a,s) -> (f a, s)
