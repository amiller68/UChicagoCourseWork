
lookupWithError :: Eq a => a -> [(a,b)] -> Maybe b
lookupWithError _ [] = error "key not found" 
lookupWithError a ((k,v):ps)
 | a == k = Just v
 | otherwise = lookupWithError a ps

lookupWithDefault :: Eq a => a -> b -> [(a,b)] -> b
lookupWithDefault _ b [] = b
lookupWithDefault a b ((k,v):ps)
 | a == k = v
 | otherwise = lookupWithDefault a b ps

data Dictionary a b = Dictionary b [(a,b)]




lookupInDictionary :: Eq a => a -> Dictionary a b -> b
lookupInDictionary _ (Dictionary b []) = b
lookupInDictionary a (Dictionary b ((k,v):ps)) 
 | a == k = v
 |otherwise = lookupInDictionary a (Dictionary b ps)
