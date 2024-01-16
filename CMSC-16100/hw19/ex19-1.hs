foldMap f container = foldr (\a b -> f a <> b) mempty container
foldMap f = foldr (\a b -> f a <> b) mempty
foldMap f = foldr ((<>) . f) mempty
