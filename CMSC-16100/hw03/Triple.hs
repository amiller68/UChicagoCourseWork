fstOf3 :: (a, b, c) -> a
fstOf3 (a, b ,c) = a

sndOf3 :: {- forall a b c. -} (a, b , c) -> b
sndOf3 (a, b, c) = b

thirdOf3 :: (a, b, c) -> c
thirdOf3 (_, _, c) = c

curry3 :: ((a, b , c)-> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d 
uncurry3  f (a, b, c) = f a b c

