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
        | isAscii  c = c
    rotCase base c = chr (ord base + (ord c - ord base + n)  `mod` 26)

rotVig :: String -> String -> String
rotVig n = map rotChar where
  rotChar c
    | isLower c = rotCase 'a' c
    | isUpper c = rotCase 'A' c
    | isAscii  c = c
  rotCase base c = chr (ord base + (ord c - ord base + (encrypt n))  `mod` 26)

derotVig :: String -> String -> String
derotVig n = map rotChar where
  rotChar c
    | isLower c = rotCase 'a' c
    | isUpper c = rotCase 'A' c
    | isAscii  c = c
  rotCase base c = chr (ord base + (ord c - ord base - (decrypt n))  `mod` 26)

encrypt :: String -> Int
encrypt key = sum (map ord key)

decrypt :: String -> Int
decrypt key = (encrypt key) - 45



rotStdin :: Int -> IO ()
rotStdin = interact . rot

rotVigStdin :: String -> IO ()
rotVigStdin = interact . rotVig

derotVigStdin :: String -> IO ()
derotVigStdin = interact . derotVig


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
      | x =~ "^-?[a-z]+$" -> rotVigStdin (read x)
--      | x =~ "^-[a-z]+$" -> derotVigStdin (read x)
      | otherwise     -> usage
    _   -> usage
