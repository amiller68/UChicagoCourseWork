module Main where



import Data.List
import TimeRanges
import SimpleTime

---- Input/Output Interface ----

-- Example: splitOn ',' "abc,def,ghi"  => ["abc", "def", "ghi"]
splitOn :: Char -> String -> [String]
splitOn splitChar []    = [[]]
splitOn splitChar (headChar:restChars)
    | splitChar == headChar = [[]] ++ splitOn splitChar restChars
    | otherwise             = (headChar:currentWord):restWords
    where
        currentWord:restWords = splitOn splitChar restChars

-- Vanilla Haskell doesn't allow instances on type synonyms,
-- so we can't make customized Show/Read instances.

readDTR :: String -> DTR
readDTR = map read . splitOn ','

showDTR :: DTR -> String
showDTR = concat . intersperse "," . map show . normalizeDTR

-- Combine touching/overlapping regions and remove NoTime records.
-- Inverting twice effectively combines overlapping regions.
normalizeDTR :: DTR -> DTR
normalizeDTR dtr
    | simplified == [] = [NoTime]
    | otherwise        = simplified
    where
        inverse = difference allTimeDTR
        simplified = sort . filter (/= NoTime) . inverse . inverse $ dtr

processLine :: String -> String
processLine line =
    -- Example: words "abc def ghi" => ["abc", "def", "ghi"]
    case words line of
        "intersection":dtrStrs -> showDTR $ intersectAll $ map readDTR dtrStrs
        "union":dtrStrs        -> showDTR $ unionAll $ map readDTR dtrStrs
        "difference":dtrStrs   -> showDTR $ differenceAll $ map readDTR dtrStrs
        "disjoint":dtrStrs     -> show $ areAllDisjoint $ map readDTR dtrStrs
        "equal":dtrStrs        -> show $ areAllEqual $ map readDTR dtrStrs
        _                      -> "Invalid input"

main :: IO ()
main = do
    line <- getLine-- <--- Write code here to get a line from the input
  --  hFlush stdin
    case line of -- Examine the line
        'q':_ -> return () -- "q" or "quit" to quit
        _     -> do
                  putStrLn $ processLine line
                  main
                --  processLine (read line)-- <--- Process and output the line here
                --  main -- Repeat
