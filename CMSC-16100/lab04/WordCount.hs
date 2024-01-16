import Data.List hiding (insert)
import InputHandling

main :: IO ()
main = getContents <&> cleanInput <&> makeTrie <&> getKeyNodes <&> unlines >>= putStr

data Trie a = Node a [Trie a] | KeyNode a Int [Trie a]

empty :: Trie String
empty = Node "" []

--Makes a trie with a set of input
makeTrie :: [String] -> Trie String
makeTrie = foldr insert empty

--Inserts value into a Trie top-down
insert :: String -> (Trie String) -> (Trie String)
-- Inserting an empty [char] into a node should turn that Node into a keyNode
insert [] (Node key children) = KeyNode key 1 children
-- Insertng an empty [char] into a KeyNode should incremnt that KeyNode
insert [] (KeyNode key val children) = KeyNode key (val + 1) children
insert (letter:letters) (Node key children)
  --If the head of a set of input is a child of a Node it should insert the tail of that input into the correct child
  | (key ++ [letter]) `elem` (map getKey children) = Node key $ sortOn getKey (checkChildren (key ++ [letter]) letters children)
  --Otherwise we just want to add a new path and populate it with the leftover input
  | otherwise = Node key $ sortOn getKey ((insert letters (Node (key ++ [letter]) [])) : children)
insert (letter:letters) (KeyNode key val children)
  --The same idea holds for KeyNodes, we don't increment here because we havent reached the end of the input
  | (key ++ [letter]) `elem` (map getKey children) = KeyNode key (val) $ sortOn getKey (checkChildren (key ++ [letter]) letters children)
  | otherwise = KeyNode key (val) $ sortOn getKey ((insert letters (Node (key ++ [letter]) [])) : children)

--Used to place data into the right places
checkChildren :: String -> String -> [Trie String] -> [Trie String]
checkChildren wordIndex wordTail (child:children)
  | wordIndex == (getKey child) = insert wordTail child : children
  | otherwise = child : (checkChildren wordIndex wordTail children)

--retrieves the key value of a node
getKey :: Trie String -> String
getKey (Node a trie) = a
getKey (KeyNode a uses trie) = a

--Converts all of the keynodes into a readable list
getKeyNodes :: Trie String -> [String]
getKeyNodes (Node key []) = []
getKeyNodes (Node key children) = [] ++ concatMap getKeyNodes children
getKeyNodes (KeyNode key val []) = [key ++ " " ++ show val]
getKeyNodes (KeyNode key val children) = [key ++ " " ++ show val] ++ concatMap getKeyNodes children
