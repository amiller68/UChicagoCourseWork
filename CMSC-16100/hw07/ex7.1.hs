data ABCD
    = A { foo :: String, bar :: Int }
    | B { foo :: String, baz :: () }
    | C Int
    | D
    deriving (Show, Eq)

-- | foo bar and baz are all names for aspects of A and B
