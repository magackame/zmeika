module Main where


import System.Random
import Control.Exception
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Char
import System.Process

import Snake (Snake)
import qualified Snake
import Vec2 (Vec2(..))
import qualified Vec2
import Direction (Direction(..))
import qualified Direction
import World (World)
import qualified World


data State
  = Playing
  | Over


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  actualDirection <- newMVar D
  direction <- newMVar D

  forkIO $ inputThread actualDirection direction

  startGame actualDirection direction


game :: State -> Vec2 -> Snake -> MVar Direction -> MVar Direction -> IO ()
game state foodPosition snake actualDirection direction = do
  let snakeLength = length snake

  case state of
    Playing -> do
      dir <- readMVar direction

      _ <- takeMVar actualDirection
      putMVar actualDirection dir

      putStrLn $ "Snake length: " ++ show snakeLength

      let world = World.drawFood foodPosition $ World.drawSnake snake $ World.emptyWorld rowCount columnCount

      putStrLn $ concat world

      (foodPosition, snake) <- checkFoodCollision foodPosition snake
      let state = checkCollision snake

      threadDelay 200000

      clear
      game state foodPosition (Snake.move dir snake) actualDirection direction

    Over -> do
      putStrLn $ "Game Over\nYour length was " ++ show snakeLength ++ "\nPress any key to continue..." 
      _ <- getChar

      startGame actualDirection direction


checkCollision :: Snake -> State
checkCollision (head:tail) =
  let
    (Vec2 x y) = head
  in
  if x <= 0 || y <= 0 || x >= columnCount - 1 || y >= rowCount - 1 || head `elem` tail
  then
    Over

  else
    Playing


checkFoodCollision :: Vec2 -> Snake -> IO (Vec2, Snake)
checkFoodCollision foodPosition snake =
  if head snake == foodPosition
  then do
    foodPosition <- generateFoodPosition snake

    return (foodPosition, Snake.elongate snake)

  else
    return (foodPosition, snake)


generateFoodPosition :: Snake -> IO Vec2
generateFoodPosition snake = do
  x <- randomRIO (2, columnCount - 2)
  y <- randomRIO (2, rowCount - 2)

  let foodPosition = Vec2 x y

  if foodPosition `elem` snake
  then
    generateFoodPosition snake

  else
    return foodPosition


startGame :: MVar Direction -> MVar Direction -> IO ()
startGame actualDirection direction = do
  foodPosition <- generateFoodPosition snake

  _ <- takeMVar actualDirection
  putMVar actualDirection D

  _ <- takeMVar direction
  putMVar direction D

  game Playing foodPosition snake actualDirection direction
  where
    snake = [Vec2 1 3, Vec2 1 2, Vec2 1 1, Vec2 2 1, Vec2 3 1, Vec2 4 1, Vec2 5 1, Vec2 6 1]


inputThread :: MVar Direction -> MVar Direction -> IO ()
inputThread actualDirection direction = do
  key <- getKey

  let dir = Direction.fromKey key
  updateDirection actualDirection direction dir

  inputThread actualDirection direction


updateDirection :: MVar Direction -> MVar Direction -> Maybe Direction -> IO ()
updateDirection actualDirection direction dir =
  case dir of
    Just dir -> do
      _ <- takeMVar direction
      prevDir <- readMVar actualDirection

      if prevDir == Direction.opposite dir
      then
        putMVar direction prevDir

      else
        putMVar direction dir

    Nothing ->
      return ()
  

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char : chars)


clear :: IO ()
clear = callCommand "clear"


rowCount :: Int
rowCount = 25


columnCount :: Int
columnCount = 80
