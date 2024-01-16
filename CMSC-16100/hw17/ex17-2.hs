import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser s = Parser { runParser :: String -> [(s,String)] }

string :: String -> Parser String
string str = Parser $ \s -> [(t,u) | let (t,u) = splitAt (length str) s, str == t]

data ComplexInt = ComplexInt Int Int
  deriving (Show)

parseInt = read <$> some digit
skipParens = const () <$> many parens
skipParens2 = const () <$> many parens2
skipCommas = const () <$> some comma


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
        [] -> []
        a:as -> [(a,as) | p a]

char :: Char -> Parser Char
char c = satisfy (c==)

alpha, digit, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
space = satisfy isSpace
parens = char '('
parens2 = char ')'
comma = char ','

instance Functor Parser where
        fmap f p = Parser $ \s ->
            [(f a,t) | (a,t) <- runParser p s]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  af <*> aa = Parser $ \s ->
      [ (f a,u)
      | (f,t) <- runParser af s
      , (a,u) <- runParser aa t
      ]

instance Alternative Parser where
    empty = Parser $ \s -> []
    p1 <|> p2 = Parser $ \s ->
        runParser p1 s ++ runParser p2 s

instance Monad Parser where
                p >>= g = Parser $ \s ->
                    [ (b,u)
                    | (a,t) <- runParser p s
                    , (b,u) <- runParser (g a) t
                    ]

instance MonadPlus Parser


parseComplexInt :: Parser ComplexInt
parseComplexInt = do
  skipParens
  a <- parseInt
  b <- optional skipCommas
  case b of
    Nothing -> pure $ ComplexInt a 0
    otherwise -> do
      c <- parseInt
      skipParens2
      pure $ ComplexInt a c
    --(int1, int2) -> pure $ ComplexInt int1 int2

instance Read ComplexInt where
  readsPrec _ = runParser parseComplexInt
