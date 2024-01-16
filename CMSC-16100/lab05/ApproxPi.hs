import           RandState
import           State
import           System.Environment
import           System.IO
import           System.Random
import           Control.Monad


randR :: (Random a) => (a, a) -> RandState a
randR (low, high) =
      do
      gen <- get
      let (res, gen') = randomR (low, high) gen
      put gen'
      return res


piTrial :: RandState Bool
piTrial = do
    x <- randR (-1 :: Double, 1 :: Double)
    y <- randR (-1 :: Double, 1 :: Double)
    return $ 1 >= sqrt (x^2 + y^2)

bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials nTimes randState = do
  trials <-  (sequence $ take nTimes (repeat randState))
  return $ length $ filter id trials


approxPi :: Int -> RandState Double
approxPi nTimes = do
  successes <- bernoulliTrials nTimes piTrial
  return $ 4.0 * (fromIntegral successes :: Double) / (fromIntegral nTimes :: Double)


main :: IO ()
main = do
     gen  <- newStdGen
     args <- getArgs
     case args of
       [arg] -> putStrLn $ show $ fst $ runState (approxPi $ read arg) gen
       otherwise -> putStrLn $ "That's bad input u dummy"
