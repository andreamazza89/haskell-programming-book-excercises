module Morra where

import System.Random (randomRIO)
import Control.Monad.State (StateT, get, runStateT, put)
import Control.Monad.IO.Class (liftIO)

-- GameState

data GameState =
  Playing GameMode ToDo ([Move])
  deriving (Show)

data ToDo =
    PlayHuman
  | PlayComputer
  | PrintGameState
  | PrintInterstitialScreen
  | ShowIntroduction
  deriving (Eq, Show)

data GameMode =
    HumanVsHuman
  | HumanVsComputer
  deriving (Show)

newtype Move =
  Move Int
  deriving (Show)

data Winner =
    P1
  | P2

parseMove :: String -> Maybe Move
parseMove input =
  case reads input of
    [(numberPicked, _)] -> parseMove_ numberPicked
    _ -> Nothing

parseMove_ :: Int -> Maybe Move
parseMove_ numberPicked =
  if numberPicked >= 0 && numberPicked <= 5 then
    Just (Move numberPicked)
  else
    Nothing

playMove :: Move -> GameState -> GameState
playMove move (Playing mode toDo moves) =
  case mode of
    HumanVsHuman ->
      let toDo' =
                  case toDo of
                    PlayHuman -> if (even $ length newMoves) then PrintGameState else PrintInterstitialScreen
                    PlayComputer -> undefined
                    PrintGameState -> undefined
                    ShowIntroduction -> undefined
                    PrintInterstitialScreen -> PrintGameState
      in
      Playing mode toDo' newMoves
    HumanVsComputer ->
      let toDo' =
                  case toDo of
                    PlayHuman -> PlayComputer
                    PlayComputer -> PrintGameState
                    PrintGameState -> undefined
                    ShowIntroduction -> undefined
                    PrintInterstitialScreen -> undefined
      in
      Playing mode toDo' newMoves
  where
    newMoves = move : moves

getToDo :: GameState -> ToDo
getToDo (Playing _ toDo _) =
  toDo

newRound :: GameState -> GameState
newRound (Playing mode _ moves) =
  Playing mode PlayHuman moves

startGame :: GameState -> GameState
startGame (Playing mode _ moves) =
  Playing mode PlayHuman []

showMove :: Move -> String
showMove (Move numberPicked) =
  show numberPicked

showLastRound :: GameState -> String
showLastRound (Playing _ _ []) =
  "No moves made yet"
showLastRound (Playing _ _ moves)
  | odd $ length moves = "Move is ongoing - who's next?"
showLastRound (Playing mode _ (computer : player : xs)) =
  case (lastRoundsWinner computer player, mode) of
    (P1, HumanVsComputer) -> "Player won the last round! "
    (P2, HumanVsComputer) -> "Computer won the last round! "
    (P1, HumanVsHuman) -> "Player 1 won the last round! "
    (P2, HumanVsHuman) -> "Player 2 won the last round! "

lastRoundsWinner :: Move -> Move -> Winner
lastRoundsWinner (Move computer) (Move player)
  | odd (computer + player) = P1
  | even (computer + player) = P2

-- IO stuff

main :: IO ()
main = do
  clearScreen
  fst <$> runStateT (loop ShowIntroduction) (Playing HumanVsHuman ShowIntroduction [])

loop :: ToDo -> StateT GameState IO ()
loop PlayHuman =
  playHuman >>= loop
loop PlayComputer =
  playComputer >>= loop
loop PrintGameState =
  printState >>= loop
loop ShowIntroduction =
  showIntro >>= loop
loop PrintInterstitialScreen =
  printInterstitial >>= loop

humanInput :: String -> (String -> Maybe a) -> IO a
humanInput prompt parse = do
  putStr prompt
  rawInput <- getLine
  case (parse rawInput) of
    Just a ->
      return a
    Nothing ->
       putStrLn "Could not parse input - please try again" >> humanInput prompt parse

playHuman :: StateT GameState IO ToDo
playHuman = do
  humanMove <- liftIO $ humanInput "Pick a number (0-5): " parseMove
  makeMove humanMove

playComputer :: StateT GameState IO ToDo
playComputer = do
  computerMove <- liftIO $ Move <$> randomRIO (0, 5)
  liftIO $ putStrLn $ "Computer picked " ++ showMove computerMove
  makeMove computerMove

makeMove :: Move -> StateT GameState IO ToDo
makeMove move = do
  newState <- updateState_ $ playMove move
  return $ getToDo newState

printState :: StateT GameState IO ToDo
printState = do
  state <- get
  liftIO $ putStrLn $ showLastRound state
  getToDo <$> updateState_ newRound

showIntro :: StateT GameState IO ToDo
showIntro = do
  liftIO $ putStrLn "welcome to game of Morra. Player is odds, Computer evens; player goes first."
  getToDo <$> updateState_ startGame

printInterstitial :: StateT GameState IO ToDo
printInterstitial = do
  liftIO $ clearScreen
  liftIO $ putStrLn "~~~~~~~~~~~~~~ interstitial screen: pass the computer to your opponent ~~~~~~~~~~~~~~~~"
  playHuman

updateState_ :: (GameState -> GameState) -> StateT GameState IO GameState
updateState_ updateFunction = do
  state <- get
  let newState = updateFunction state
  put newState
  return newState

clearScreen :: IO ()
clearScreen =
  putStr "\ESC[2J" >> putStr "\ESC[H"