module Main where

import Control.Monad.State
import Data.Map (Map,(!),singleton,unionsWith)
import System.Random

type RandState = State StdGen
type Model = (String,Map String [Maybe String])

buildModel :: [String] -> Model
buildModel xs@(x:_) = (x,unionsWith (++) . transitions $ xs) where
    transitions (y:ys@(y':_)) = singleton y [Just y'] : transitions ys
    transitions [y] = [singleton y [Nothing]]
    transitions [] = error "Impossible error"
buildModel [] = error "Empty model"

runModel :: Model -> RandState [String]
runModel (start,wordmap) = iter start where
    iter word = (word:) <$> do
        maybeNext <- select $ wordmap ! word
        case maybeNext of
            Just nextWord -> iter nextWord
            Nothing -> pure []

roll :: Int -> RandState Int
roll n = state $ randomR (1,n)

select :: [a] -> RandState a
select as = (as !!) . (subtract 1) <$> roll (length as)

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter current (nextWord:ys)
        | length current + length nextWord + 1 > n = current ++ "\n" ++ linefill n (nextWord:ys)
        | otherwise                   = iter (current ++ " " ++ nextWord) ys
    iter current [] = current ++ "\n"

--Reveals range of the gettysburg address is ~39 charahcters to ~10000
main :: IO ()
main = do
    input <- getContents
    list <- replicateM 1000 $ do
      gen <- getStdGen
      let model = buildModel (words input)
          disassociatedPress = evalState (runModel model) gen
      newStdGen
      pure $ length $ linefill 72 $ disassociatedPress
    putStrLn $  "Max string length: " ++ (show $ maximum list)
    putStrLn $  "Min string length: " ++ (show $ minimum list)
