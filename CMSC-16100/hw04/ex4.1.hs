import Prelude hiding (even)

even :: Integral n => n -> Bool
even = (==  0) . (mod 2) 