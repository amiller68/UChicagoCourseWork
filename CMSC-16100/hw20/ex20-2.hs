module Applic where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Applicative (MaybeT m) where
    pure = MaybeT . pure. Just
    (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ (<*>) (fmap (<*>) mmf) mma
