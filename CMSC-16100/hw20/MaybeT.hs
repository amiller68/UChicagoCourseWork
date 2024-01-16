module MaybeT where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Char
import Func
import Applic

readInt0 :: IO (Maybe Int)
readInt0 = do
  s <- getLine
  if all isDigit s
    then pure $ Just (read s)
    else pure Nothing

readInt :: IO (Maybe Int)
readInt = do
  s <- getLine
  pure $ do
    guard (all isDigit s)
    Just (read s)

addThree0 :: IO (Maybe Int)
addThree0 = do
  mi <- readInt
  mj <- readInt
  mk <- readInt
  case (mi, mj, mk) of
    (Just i, Just j, Just k) -> pure $ Just (i+j+k)
    _                        -> pure Nothing

addThree1 :: IO (Maybe Int)
addThree1 = do
  mi <- readInt
  mj <- readInt
  mk <- readInt
  pure $ do
    i <- mi
    j <- mj
    k <- mk
    pure $ i + j + k

addThree2 :: IO (Maybe Int)
addThree2 = do
  mi <- readInt
  case mi of
    Nothing -> pure Nothing
    Just i  -> do
      mj <- readInt
      case mj of
        Nothing -> pure Nothing
        Just j -> do
          mk <- readInt
          case mk of
            Nothing -> pure Nothing
            Just k  -> pure $ Just (i+j+k)

bindMonadPlusMaybe :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
action `bindMonadPlusMaybe` f = do
  ma <- action
  case ma of
    Nothing -> pure Nothing
    Just a  -> f a

pureMonadPlusMaybe :: Monad m => a -> m (Maybe a)
pureMonadPlusMaybe a = pure $ Just a

addThree :: IO (Maybe Int)
addThree =
  readInt `bindMonadPlusMaybe` \i ->
  readInt `bindMonadPlusMaybe` \j ->
  readInt `bindMonadPlusMaybe` \k ->
  pureMonadPlusMaybe (i+j+k)

----------------------------------------------------------------------

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
 -- return :: a -> MaybeT m a
    return = MaybeT . return . Just

 -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    MaybeT mma >>= f = MaybeT $ do
      ma <- mma
      case ma of
        Nothing -> pure Nothing
        Just a  -> runMaybeT $ f a
{-}
instance Functor m => Functor (MaybeT m) where
  --what I want
  --fmap :: (Maybe a -> Maybe b) -> m (Maybe a) -> m (Maybe b)
  --What f Needs to do
  --f :: Maybe a -> Maybe b
  --Which looks an awful lot like (fmap f) when applied to Maybe
    fmap f (MaybeT mma) = MaybeT $ fmap (fmap f) mma
-}
{-}
instance Monad m => Applicative (MaybeT m) where
    pure = MaybeT . pure. Just
    --(<*>) :: m (Maybe (a -> b)) -> m (Maybe a) -> m (Maybe b)
    --I think I want to use apply and fmap
    --(<*>) mmf :: m (Maybe a) -> m (Maybe b)
    --(MaybeT mmf) <*> (MaybeT mma) = MaybeT $(<*>) <$> mmf <*> mma
    (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ (<*>) (fmap (<*>) mmf) mma
    --Wow that worked
    --Now to understand why lol
    --(<*> mmf) :: m (Maybe a) -> m (Maybe b)
    --fmap (<*>) mmf :: m (Maybe a -> Maybe b) -> m (Maybe a) -> m (Maybe b)
-}


instance MonadTrans MaybeT where
 -- lift :: Monad m => m a -> MaybeT m a
 -- lift = liftMaybeT
 -- lift ma = MaybeT (fmap Just ma)
    lift = MaybeT . fmap Just

instance (Monad m, Alternative m) => Alternative (MaybeT m) where
 -- empty :: MaybeT m a
    empty = MaybeT empty

 -- (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
    MaybeT mma <|> MaybeT mmb = MaybeT $ mma <|> mmb

----------------------------------------------------------------------


liftMaybeT :: Monad m => m a -> MaybeT m a
liftMaybeT ma = MaybeT (fmap Just ma)

guardMaybeT :: Monad m => Bool -> MaybeT m ()
guardMaybeT True  = MaybeT $ pure $ Just ()
guardMaybeT False = MaybeT $ pure Nothing

maybeReadInt0 :: MaybeT IO Int
maybeReadInt0 = do
  s <- liftMaybeT getLine
  if all isDigit s
    then pure $ read s
    else MaybeT $ pure Nothing

maybeAddThree0 :: MaybeT IO Int
maybeAddThree0 = do
  i <- maybeReadInt0
  j <- maybeReadInt0
  k <- maybeReadInt0
  pure $ i+j+k

maybeReadInt1 :: MaybeT IO Int
maybeReadInt1 = do
  s <- liftMaybeT getLine
  guardMaybeT $ all isDigit s
  pure $ read s

maybeReadInt :: MaybeT IO Int
maybeReadInt = do
  s <- lift getLine
  guard $ all isDigit s
  pure $ read s

maybeAddThree :: MaybeT IO Int
maybeAddThree = do
  i <- maybeReadInt
  j <- maybeReadInt
  k <- maybeReadInt
  pure $ i+j+k
