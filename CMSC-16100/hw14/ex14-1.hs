import Data.Monoid
import Control.Monad

newtype Writer w a = Writer (a, w)

instance Functor (Writer w) where
  fmap f (Writer (a,w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a,mempty)
  (Writer (fa,fw)) <*> (Writer (xa,xw)) = Writer (fa xa, fw <> xw)

instance {-forall w. -} Monoid w => Monad (Writer w) where
  return a = Writer (a, mempty)
  (Writer (a , w1)) >>= f =
      let Writer (b, w2) = f a in
      Writer (b, w1 <> w2)
