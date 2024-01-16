{- A module for producing "pretty" HTML. -}

module Pretty where

import Prelude hiding (head)

import Control.Monad.State

type Document = State (Int, String)

{- Append a String to the HTML document. -}
tab = "  "

string :: String -> Document ()
string t = modify (\(tabs,s) -> (tabs, s ++ t))


{- Append a newline, indenting the subsequent text. -}

newline :: Document ()
newline = modify (\(tabs, s) -> (tabs, s ++ "\n" ++ concat ( take tabs $repeat tab ) ))

{- Render a Document as Text -}

render :: Document a -> String
render doc =  snd $ execState doc (0, "")
-- concat (take (fst $ execState doc (0, "")) (repeat tab)) ++
{- Increase the indentation level. -}

indent :: Document ()
indent = modify (\(tabs,s) -> (tabs+1, s))

{- Decrease the indentation level. -}

exdent :: Document ()
exdent = modify (\(tabs,s) -> (tabs-1, s))

{- An HTML tag which indents its content -}

type HTML = Document ()

tag :: String -> HTML -> HTML
tag t html = do
    newline
    inlineTag t $ do
        indent
        html
        exdent
        newline

{- An HTML tag which does not indent its content -}

inlineTag :: String -> HTML -> HTML
inlineTag t html = do
    string $ "<" ++ t ++ ">"
    void html
    string $ "</" ++ t ++ ">"

{- An HTML tag which presents its content on a new line -}

onelineTag :: String -> HTML -> HTML
onelineTag t html = newline >> inlineTag t html

{- Provide the functionality of the standard HTML tags -}

html, head, title, body, p, i, b, h1, h2, h3, h4, ol, ul, li, table, tr, th, td
    :: HTML -> HTML
html  = tag "html"
head  = tag "head"
title = onelineTag "title"
body  = tag "body"
p     = onelineTag "p"
i     = inlineTag "i"
b     = inlineTag "b"
h1    = onelineTag "h1"
h2    = onelineTag "h2"
h3    = onelineTag "h3"
h4    = onelineTag "h4"
ol    = tag "ol"
ul    = tag "ul"
li    = onelineTag  "li"
table = tag "table"
tr    = tag "tr"
th    = tag "th"
td    = tag "td"

testDoc :: Document ()
testDoc = do
  indent
  string "eh"

doc :: HTML
doc =
  html $ do
    head $ do
      title $ string "Hello, world!"
    body $ do
      h1 $ string "Greetings"
      p $ string "Hello, world!"
