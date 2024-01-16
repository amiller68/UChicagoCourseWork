module Main where
import System.Environment
import Data.Char

main :: IO ()
main = do
  let cap (first:rest) = toUpper first : rest
  putStrLn "Hello. I am a HAL 9000 series computer."
  name <- getEnv  "USER"
  putStrLn $ "Good morning, " ++ (cap name) ++ "."
