-- | The annoying "frog" program.

module Main where

import System.IO



main :: IO ()
main = do
  hFlush stdout
  nstr <- getContents
  eh <- lines nstr
  putStr $ eh
  main
