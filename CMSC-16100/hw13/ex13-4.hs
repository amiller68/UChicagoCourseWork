import Control.Applicative


altconcat :: Alternative f => [f a] -> f a
altconcat = foldr (<|>) empty
