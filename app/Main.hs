

module Main (main) where

import MyLib
import qualified Data.List.NonEmpty as L
import System.Random (getStdGen, StdGen)
import System.Console.ANSI
import System.IO (hSetEcho, hSetBuffering, stdin, stdout, BufferMode(NoBuffering), hReady)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Prelude hiding (Left, Right)

-- Game state
data GameState = GameState
  { gameSnake :: Snake
  , gameBoard :: Board
  , gameGen :: StdGen
  , gameScore :: Int
  , gameRunning :: Bool
  } deriving (Show)

-- Initialize game
initGame :: IO GameState
initGame = do
  gen <- getStdGen
  let board = Board { xdim = 40, ydim = 30, fruit = Point 10 7 }
      snake = Snake { body = Point 5 5 L.:| [Point 4 5, Point 3 5], direction = Right }
  return GameState
    { gameSnake = snake
    , gameBoard = board
    , gameGen = gen
    , gameScore = 0
    , gameRunning = True
    }

-- Render the game board
renderGame :: GameState -> IO ()
renderGame gs = do
  clearScreen
  setCursorPosition 0 0

  -- Draw top border
  putStrLn $ "+" ++ replicate (xdim (gameBoard gs)) '-' ++ "+"

  -- Draw board rows
  let board = gameBoard gs
      snake = gameSnake gs
      snakePositions = L.toList (body snake)
      fruitPos = fruit board

  sequence_ $ do
    y' <- [0 .. ydim board - 1]
    return $ do
      putStr "|"
      sequence_ $ do
        x' <- [0 .. xdim board - 1]
        let pos = Point x' y'
        return $ putStr $
          if pos `elem` snakePositions then
            if pos == L.head (body snake) then "O"  -- Head
            else "*"  -- Body
          else if pos == fruitPos then "@"  -- Fruit
          else " "  -- Empty
      putStrLn "|"

  -- Draw bottom border
  putStrLn $ "+" ++ replicate (xdim board) '-' ++ "+"

  -- Show game info
  putStrLn $ "Score: " ++ show (gameScore gs)
  putStrLn $ "Direction: " ++ show (direction snake)
  putStrLn "Controls: WASD to move, Q to quit"

  -- Check game over
  unless (snakeAlive board snake) $ do
    putStrLn ""
    putStrLn "GAME OVER!"
    putStrLn $ "Final Score: " ++ show (gameScore gs)

-- Handle input
handleInput :: GameState -> IO GameState
handleInput gs = do
  hasInput <- hReady stdin
  if hasInput
    then do
      char <- getChar
      case char of
        'w' -> return gs { gameSnake = (gameSnake gs) { direction = Up } }
        's' -> return gs { gameSnake = (gameSnake gs) { direction = Down } }
        'a' -> return gs { gameSnake = (gameSnake gs) { direction = Left } }
        'd' -> return gs { gameSnake = (gameSnake gs) { direction = Right } }
        'q' -> return gs { gameRunning = False }
        _   -> return gs
    else return gs

-- Update game state
updateGame :: GameState -> IO GameState
updateGame gs = do
  let snake = gameSnake gs
      board = gameBoard gs
      gen = gameGen gs

  if not (snakeAlive board snake)
    then return gs { gameRunning = False }
    else do
      let oldLength = L.length (body snake)
          (newSnake, newBoard, newGen) = moveSnake gen snake board
          newLength = L.length (body newSnake)
          scoreIncrease = if newLength > oldLength then 10 else 0
          newScore = gameScore gs + scoreIncrease

      return GameState
        { gameSnake = newSnake
        , gameBoard = newBoard
        , gameGen = newGen
        , gameScore = newScore
        , gameRunning = gameRunning gs
        }

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop gs = do
  renderGame gs

  if not (gameRunning gs) || not (snakeAlive (gameBoard gs) (gameSnake gs))
    then do
      putStrLn "Press any key to exit..."
      _ <- getChar
      return ()
    else do
      newGs <- handleInput gs >>= updateGame
      threadDelay 100000  -- 300ms delay (adjust for game speed)
      gameLoop newGs

-- Setup terminal and run game
main :: IO ()
main = do
  -- Configure terminal
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor

  putStrLn "Welcome to Snake Game!"
  putStrLn "Press any key to start..."
  _ <- getChar

  -- Run game
  initGame >>= gameLoop

  -- Cleanup
  showCursor
  clearScreen
  setCursorPosition 0 0
  putStrLn "Snake died :-("
