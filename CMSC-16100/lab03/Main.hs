import System.IO
import Lab3


main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     putStrLn $ show $ eval $ parse $ tokenize line
     main
