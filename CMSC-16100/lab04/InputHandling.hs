module InputHandling where

--Had issue Where my cleanInput included empty strings, found way to fix @
--https://stackoverflow.com/questions/10414861/removing-every-instance-of-the-empty-list-from-a-list-of-list

import Data.Char
import Data.List

cleanInput :: String -> [String]
cleanInput = filter (not . null) . (splitOn (isUndesiredInput)) . map toLower

isUndesiredInput :: Char -> Bool
isUndesiredInput x =
    if ((isLetter x) || (x == '\'') || (x == '-')) then False
    else True


--Lifted and modified from Lab02
splitOn :: (Char -> Bool) -> String -> [String]
splitOn splitChar []    = [[]]
splitOn splitChar (headChar:restChars)
    | (splitChar headChar) = [[]] ++ splitOn splitChar restChars
    | otherwise             = (headChar:currentWord):restWords
    where
        currentWord:restWords = splitOn splitChar restChars

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
