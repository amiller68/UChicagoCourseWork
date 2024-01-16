applyMaybeFancy :: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybeFancy _ Nothing  = Nothing
applyMaybeFancy f (Just x) = f x

andThenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
andThenMaybe = flip applyMaybeFancy

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mma = (Just mma) `andThenMaybe` (\(Just x) -> x)
