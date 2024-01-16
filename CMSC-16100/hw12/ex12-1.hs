module Main where

import System.IO

main :: IO ()
main = interact $ enumerate
  where
    enumerate  = unlines . zipWith (++) indices . lines
      where
        indices = (++ ". ") <$> (map show [1..])

-- Question: is there a way to simply this so I don't use where twice?
-- For some reason let, in was not working
-- also I tried using applicatives and functor for this to make a neat pipeline
-- but I kept getting type check errors. Would whoever's grading this help me see what
-- I'd need to do to make that work?
-- Thanks!
