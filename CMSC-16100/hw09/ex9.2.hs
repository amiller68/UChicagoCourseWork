type Pair a b = (,) b a

instance Functor (Pair b) where
  fmap f (x, y) = (f x, y)

{-
• Illegal instance declaration for ‘Functor (Pair b)’
        (All instance types must be of the form (T t1 ... tn)
         where T is not a synonym.
         Use TypeSynonymInstances if you want to disable this.)
    • In the instance declaration for ‘Functor (Pair b)’

-}

--We get this error because
