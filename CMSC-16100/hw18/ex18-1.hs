--Recieved Insight from Jerome, also looked at source code

import Text.ParserCombinators.ReadP
import           Control.Monad


greedy :: ReadP a -> ReadP [a]
greedy reader = greedy' reader <++ return []
  where greedy' reader = liftM2 (:) reader (greedy reader)
