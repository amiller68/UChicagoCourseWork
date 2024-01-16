module Lab3 where

import Data.List
import Data.Char
import Debug.Trace

-- Numerb (-3)
-- Div (Number 13) (Number 6)

data ArithExp
  = Number Int
  | Plus (ArithExp) (ArithExp)
  | Mult (ArithExp) (ArithExp)
  | Div (ArithExp) (ArithExp)
  deriving (Show, Eq)

eval :: ArithExp -> Int
eval (Number a) = a
eval (Plus a b) = eval a + eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a `quot` eval b


data Token
  = NumTok Int
  | PlusTok
  | MultTok
  | DivTok
  | ParensTok [Token]
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:cs)
  | isDigit c || c == '-'
    = tokenizeInt (c:cs)
  | c == '+' = PlusTok : tokenize cs
  | c == '/' = DivTok : tokenize cs
  | c == '*'= MultTok : tokenize cs
  | c == '('
    = ParensTok (tokenize $ parensBlock cs) : tokenize (drop (length $ parensBlock cs) cs)
  | otherwise = tokenize cs
    where
      -- handle tokenization of intergers       finds the interger                     operates on the rest
      tokenizeInt (c:cs) = NumTok (read ([c] ++ (fst $ span isDigit cs))) : tokenize (snd (span isDigit cs))
      -- defines a block of the string as a full block of parenthesis
      parensBlock str = head $ dropWhile (isNotResult) possibly -- find the first substring that satisfies the constraints of a
            where
              -- breaks the string into parts with increasing numbers of included chars
              possibly = inits str
              -- tests if a str is a parens block (amount of closed parentheses is one more then the amount of open, i know
              -- it's weird that's just how I thought it made sense to do it)
              isNotResult str =
                if (countOpenParens str == (countClosedParens str) - 1) then False
                else True
                where
                  -- find the amount of open and closed parens in a string
                  countOpenParens str = length $ filter (== '(') str
                  countClosedParens str = length $ filter (== ')') str


parse :: [Token] -> ArithExp
-- Base cases for how to deal with Number Tokens and parenthesis
parse [NumTok int] = Number int
parse [ParensTok tokens] = parse tokens
-- Creats an arithmatic expression from a Token list
parse expression = tokenToArith opThatNeedsToHappenLast leftOfThatOperator rightOfThatOperator
  where
    opThatNeedsToHappenLast = findLastOp expression
    -- creates a list of all the tokens to the LEFT of the LAST occurence of the lowest precedence operator
    leftOfThatOperator = reverse $ tail (dropWhile (/=opThatNeedsToHappenLast )(reverse (expression)))
    -- creates a list of all the tokens to the RIGHT of the LAST occurence of the lowest precedence operator
    rightOfThatOperator = reverse (takeWhile (/= opThatNeedsToHappenLast) (reverse (expression)))

--Helper function to pattern match tokens to arithmatic expressions
tokenToArith :: Token -> [Token] -> [Token]-> ArithExp
tokenToArith PlusTok x y = Plus (parse x) (parse y)
tokenToArith MultTok x y = Mult (parse x) (parse y)
tokenToArith DivTok x y = Div (parse x) (parse y)

-- Helper Function that finds the operator of least precedence in a list of tokens
findLastOp :: [Token] -> Token
findLastOp tokens =
  if PlusTok `elem` tokens then PlusTok
  else if DivTok `elem` tokens then DivTok
  else MultTok
