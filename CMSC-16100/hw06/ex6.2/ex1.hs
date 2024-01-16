module Main where
import System.Exit
import System.IO
import System.Environment
import Data.Char
import Text.Regex.Posix


rot :: Int -> String -> String
rot n = map rotChar where
    rotChar c
        | isLower c = rotCase 'a' c
        | isUpper c = rotCase 'A' c
        | otherwise = c
    rotCase base c = chr (ord base + (ord c - ord base + n)  `mod` 26)

rotStdin :: Int -> IO ()
rotStdin n = do
  input <- getContents
  let output = rot n input
  if checkAscii input
    then putStr output
    else do
      usage

checkAscii :: String -> Bool
checkAscii [] = True
checkAscii (x:xs)
  | isAscii x = checkAscii xs
  | otherwise = False

usage :: IO ()
usage = do
  progname <- getProgName
  hPutStrLn stderr $ "usage: " ++ progname ++ " [n]"
  exitWith $ ExitFailure 255

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> rotStdin 13
    [x]
      |  x =~ "^-?[0-9]+$" -> rotStdin (read x)
      | otherwise     -> usage
    _   -> usage
