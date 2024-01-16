import Data.List
import System.IO
import InputHandling

data Permutation = Permutation {getPermutation :: String, getArrangements :: [String]}

main :: IO ()
main = getContents <&> cleanInput <&> map show . constructPermutations <&> unlines >>= putStr

instance Show Permutation where
  show permutation = intercalate ", " $ getArrangements permutation

constructPermutations :: [String] -> [Permutation]
--                     Sort list of perms on first perm   removes cases of single instances         creates a list of permutations
constructPermutations = sortOn (head . getArrangements) . filter ((> 1) . length . getArrangements) . foldr checkPerm ([])

checkPerm :: String -> [Permutation] -> [Permutation]
checkPerm word [] = [Permutation (sort word) [word]]
checkPerm word (permutation:permutations)
  --Check if the word is a known Permutation, if it is that word should be added to a permutation
  | word `isKnownPermuation` permutation = appendPermutation word permutation : permutations
  -- if it isn't check the word against the rest of the list
  | otherwise = permutation : checkPerm word permutations
  where
    --finds if the sorted word equals the base permutation of a list of permutationss
    isKnownPermuation word permutation = (== (getPermutation permutation)) $ sort word
    --adds a word to a list of equivalent permutations while deleting any repeat occurences, sorts as it goes as well
    appendPermutation word permutation = Permutation (getPermutation permutation) (sort $ nub $ [word] ++ (getArrangements permutation))
