-- | The annoying "frog" program.

module Main where

import System.IO



main :: IO ()
--main = getContents <&> enumerate >>= putStr
main = interact $ enumerate




enumerate :: String -> String
--enumerate (c:cs) = [editedLine | index <- [1..], line <- (c:cs), editedLine <- (index ++ line) ]=
enumerate txt = unlines $ zipWith (++) (map ((flip (++)) ". ") (map show [1..])) (lines txt)
--enumerate txt = (++) <$> (map show [1..]) <*> txt
-- As you can see I attempted to use applicative but I kept on getting a non terminating
--take 10 $ map ((flip (++)) ". ") (map show [1..])


infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
