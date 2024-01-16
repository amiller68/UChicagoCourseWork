module RandState where

import           State
import           System.Random

-- In order to generate pseudo-random numbers, need to pass around generator
--  state in State monad
type RandState a = State StdGen a

-- runRandom runs a RandState monad, given an initial random number generator
-- runRandom is equivalent to evalState
runRandom :: RandState a -> StdGen -> a
runRandom (State f) s =
    res
    where (res, state) = f s

-- rand is a helper function that generates a random instance of any
-- type in the Random class, using the RandState monad.
-- This function is here for pedagogical purposes. You will not use it in
-- your lab, instead defining a similar randR function.
rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x
