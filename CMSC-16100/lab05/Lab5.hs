import           RandState
import           State
import           System.Environment
import           System.IO
import           System.Random
import           Control.Monad





-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
            suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
            suitStr Spades   = "♠"
            suitStr Clubs    = "♣"
            -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
            -- suitStr Diamonds = "D"
            -- suitStr Spades   = "S"
            -- suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

testDeck = [PlayingCard King Hearts, PlayingCard Queen Hearts, PlayingCard (NumberCard 1) Hearts]

randR :: Random a => (a, a) -> RandState a
randR (low, high) =
      do
      gen <- get
      let (res, gen') = randomR (low, high) gen
      put gen'
      return res

rollTwoDice :: RandState Int
rollTwoDice = do
  dice1 <- randR (1,6)
  dice2 <- randR (1,6)
  return $ dice1 + dice2

removeCard :: Deck -> RandState (PlayingCard, Deck)
removeCard deck = do
  index <- randR (0, (length deck) - 1)
  let (top, bottom) = splitAt index deck
  return (head bottom, top ++ tail bottom)

shuffleDeck :: Deck -> RandState Deck
shuffleDeck [] = return []
shuffleDeck deck  = do
  (card, card') <- removeCard deck
  cards <- shuffleDeck card'
  return $ card : cards

shuffleADeck :: RandState Deck
shuffleADeck = shuffleDeck fullCardDeck




shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen = do
  let shuffles = sequence $ take nTimes (repeat shuffleADeck)
  putStr $ unlines $ map show $ fst $ runState shuffles gen

rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen = do
  let rolls = sequence $ take nTimes (repeat rollTwoDice)
  putStr $ unlines $ map show $ fst $runState rolls gen









-- -- BESIDES UNCOMMENTING, DO NOT MODIFY BELOW THIS LINE --

usage :: String
usage =
     "Lab 5: Randomizer\n" ++
     "\n" ++
     "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
     "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
     "\n"

main :: IO ()
main = do
     gen  <- newStdGen
     args <- getArgs
     case args of
         ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
         ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
         _                       -> putStrLn usage
