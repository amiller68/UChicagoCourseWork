newtype State s a = State { runState :: s -> (a,s) }


instance Functor (State s) where
        fmap f ma = State $ \s ->
            let (a,t) = runState ma s
            in (f a,t)

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  af <*> aa = State $ \s ->
    let (f,t) = runState af s
        (a,u) = runState aa t
    in (f a, u)

instance Monad (State s) where
--  ma >>= f = State $ \s ->
--    let (a, t) = runState ma s
--    in runState (f a) t
--  ma >>= f = State $ (\(a, s) -> (runState (f a) t)) . (s -> (a,s))
  ma >>= f = State $ (\(a,s) -> runState (f a) s) . runState ma 
