-- | Wrapping a number in Maybe

module MaybeNum where
    
 -- | Derived instance definition for Num (Maybe n) given Num n.
    
instance Num n => Num (Maybe n) where
 Just a + Just b = Just $ a + b
 _ + _ = Nothing
    
 Just a - Just b = Just $ a - b
 _ - _ = Nothing
    
 Just a * Just b = Just $ a * b
 _ * _ = Nothing
    
 negate (Just a) = Just $ negate a
 negate _ = Nothing
    
 abs (Just a) = Just $ abs a
 abs _ = Nothing
    
 signum (Just a) = Just (signum a)
 signum _ = Nothing
    
 fromInteger i = Just $ fromInteger i

 -- | safe division
    
infixl 7 //
    
(//) :: (Eq n, Integral n) => Maybe n -> Maybe n -> Maybe n
Just a // Just b
 | b == 0 = Nothing
 | otherwise = Just $ div a b



infixr 5 :

