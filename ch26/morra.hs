-- ch26/morra.hs

module Morra where

import qualified Data.Map as M
import Control.Monad.Trans.State.Lazy
import System.Random (randomRIO)
import Text.Trifecta

main :: IO ()
main = do
  putStars
  putStrLn "Welcome to Morra!"
  playerChoice <- chooseMode
  putStrLn "Player1 is odds, Player2 is evens"
  rounds <- getRounds
  s <- playNRounds rounds (mkEmptyGame playerChoice)
  putStars
  declareWinner s

getRounds :: IO Integer
getRounds = do
  putStrLn "How many rounds do you want to play?"
  putStrLn "Warning! I get smarter the more I play ..."
  response <- getLine
  case (parseString parseNum mempty response) of
    (Success x) ->
      case x > 0 of
        True  -> return x
        False -> do
          putStrLn "Defaulting to 15 rounds"
          return 15
    (Failure _) -> do
      putStrLn "Defaulting to 15 rounds"
      return 15

parseNum :: Parser Integer
parseNum = do
  x <- decimal
  return x

playNRounds :: Integer -> Game -> IO Score
playNRounds 0 (Game s _ _) = return s
playNRounds n g = do
  (_, ng) <- runStateT playRound g
  (playNRounds (n - 1) ng)

chooseMode :: IO Player
chooseMode = do
  putStars
  putStrLn "How many players? Press 1 for 1-player or 2 for 2-player"
  c <- getLine
  case (length c) > 0 && (head c) == '2' of
    True  -> return Human
    False -> return Computer

putStars :: IO ()
putStars = do
  putStrLn ""
  putStrLn "*****************"
  putStrLn ""

--

data Score =
  Score Int Int
  deriving (Eq, Show)

data Player =
    Computer
  | Human
  deriving (Eq, Show)

data Game =
  Game {
    score :: Score,
    playerHistory :: [Int],
    player2 :: Player
  } deriving (Eq, Show)

mkEmptyGame :: Player -> Game
mkEmptyGame = Game (Score 0 0) []

--

getHuman1Play :: IO Int
getHuman1Play = do
  putStr "P1: "
  getHumanPlay

getHumanPlay :: IO Int
getHumanPlay = do
  play <- getLine
  case (validatePlay play) of
    Nothing -> do
      putStrLn "You must enter a number between 1 and 5"
      getHumanPlay
    (Just x) -> do
      return x

getHuman2Play :: IO Int
getHuman2Play = do
  hidePlays 36
  putStr "P2: "
  getHumanPlay

hidePlays :: Int -> IO ()
hidePlays 0 = putStrLn "Please give the computer to Player 2"
hidePlays n = do
  putStrLn ""
  hidePlays (n - 1)

validatePlay :: String -> Maybe Int
validatePlay x =
  if length x == 1
  then M.lookup (head x) digits
  else Nothing
  where digits = M.fromList $ zip ['1'..'5'] [1..5]

getPlayer2Play :: Game -> IO Int
getPlayer2Play g =
  case (player2 g) of
    Computer -> getComputerPlay g
    Human    -> getHuman2Play 

getComputerPlay :: Game -> IO Int
getComputerPlay g =
  case findPattern g of
    Nothing -> do
      play <- randomElem [1..5]
      putStrLn $ "C: " ++ show play
      return play
    (Just x) -> do
      play <- choosePlay x
      putStrLn $ "C: " ++ show play
      return play

randomElem :: [a] -> IO a
randomElem s = do
  let ls = (length s) - 1
  i <- randomRIO (0, ls)
  return $ s !! i

findPattern :: Game -> Maybe Int
findPattern g =
  let plays = (playerHistory g)
  in case (length plays) > 3 of
    False -> Nothing
    True  -> scanForPattern (take 3 plays) plays

scanForPattern :: [Int] -> [Int] -> Maybe Int
scanForPattern _ [] = Nothing
scanForPattern plays (h:hs) =
  case (length hs) < 3 of
    True -> Nothing
    False ->
      let playBools = map even plays
          lastThree = map even (take 3 hs)
      in if playBools == lastThree
         then Just h
         else scanForPattern plays hs

choosePlay :: Int -> IO Int
choosePlay n = do
  case (even n) of
    True  -> randomElem [2, 4]
    False -> randomElem [1, 3, 5]

--

playRound :: StateT Game IO Score
playRound = StateT $ \g -> do
  playerPlay   <- getHuman1Play
  player2Play <- getPlayer2Play g
  newScore <- updateScore (score g) playerPlay player2Play
  let newGame  = updateGame g newScore playerPlay
  return (newScore, newGame)

updateGame :: Game -> Score -> Int -> Game
updateGame (Game _ ps pl) s p = Game s (p:ps) pl

updateScore :: Score -> Int -> Int -> IO Score
updateScore (Score ps cs) p' c' =
  case odd (p' + c') of
    True  -> do
      putStrLn $
        (show p') ++ " + " ++ (show c') ++ " is ODD. Point to Player 1."
      return $ Score (ps + 1) cs
    False -> do
      putStrLn $
        (show p') ++ " + " ++ (show c') ++ " is EVEN. Point to Player 2."
      return $ Score ps (cs + 1)

--
declareWinner :: Score -> IO ()
declareWinner (Score p1 p2) = do
  putStrLn $
    "Final score. P1: " ++
    (show p1) ++ " P2: " ++ (show p2)
  putStars
  if p1 > p2
  then putStrLn "Player1 wins!"
  else putStrLn "Player2 wins!"
