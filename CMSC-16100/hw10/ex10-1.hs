liftAN :: Applicative f => ([a] -> b) -> f [a] -> f b
liftAN = fmap
