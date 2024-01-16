data Tree a = Node a (Forest a) | EmptyNode
type Forest a = [Tree a]
  
preorder :: Tree a -> [a]
preorder (Node _ []) = [] 




