newtype Parser s = Parser { runParser :: String -> [(s,String)] }



satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
        [] -> []
        a:as -> [(a,as) | p a]

char :: Char -> Parser Char
--char c = satisfy (c==)
--char c = satisfy ((==) c)
--char c = satisfy $ (==) c
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
--(==) c :: Char -> Bool
--(==) :: Char -> (Char -> Bool)
--satisfy :: (Char -> Bool) -> Parser Char
-- Apply this reasoning to (.):
-- (.) :: ((Char -> Bool) -> Parser Char) -> (Char -> (Char -> Bool)) -> (Char -> Parser Char)
--Therefore
char = satisfy . (==)
