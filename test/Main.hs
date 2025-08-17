module Main (main) where

import Prelude hiding (Left, Right)
import MyLib (Point(..), Direction(..), Snake(..), Board(..), snakeAlive, moveSnake)
import qualified Data.List.NonEmpty as L
import System.Random (mkStdGen)

-- Simple test framework
data TestResult = Pass | Fail String deriving (Eq)

instance Show TestResult where
  show Pass = "PASS"
  show (Fail msg) = "FAIL: " ++ msg

test :: String -> Bool -> TestResult
test _ True = Pass
test name False = Fail name

runTests :: [(String, TestResult)] -> IO ()
runTests tests = do
  putStrLn "Running tests..."
  putStrLn ""
  mapM_ printTest tests
  putStrLn ""
  let passed = length $ filter (== Pass) (map snd tests)
  let total = length tests
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " tests passed"
  where
    printTest (name, result) = putStrLn $ name ++ ": " ++ show result

-- Test data
board10x10 :: Board
board10x10 = Board { xdim = 10, ydim = 10, fruit = Point 7 7 }

board5x5 :: Board
board5x5 = Board { xdim = 5, ydim = 5, fruit = Point 3 3 }

-- Helper to create a snake with single point
singlePointSnake :: Point -> Direction -> Snake
singlePointSnake p dir = Snake { body = L.singleton p, direction = dir }

-- Helper to create a snake with multiple points
multiPointSnake :: [Point] -> Direction -> Snake
multiPointSnake [] _ = error "Cannot create empty snake"
multiPointSnake (p:ps) dir = Snake { body = p L.:| ps, direction = dir }

-- Test cases
testSnakeAlive :: [(String, TestResult)]
testSnakeAlive = 
  [ -- Test snake in bounds
    ("Snake at origin should be alive", 
     test "origin" $ snakeAlive board10x10 (singlePointSnake (Point 0 0) Up))
    
  , ("Snake in center should be alive", 
     test "center" $ snakeAlive board10x10 (singlePointSnake (Point 5 5) Right))
    
  , ("Snake at edge should be alive", 
     test "edge" $ snakeAlive board10x10 (singlePointSnake (Point 9 9) Left))
    
    -- Test snake out of bounds - negative coordinates
  , ("Snake with negative x should be dead", 
     test "negative x" $ not $ snakeAlive board10x10 (singlePointSnake (Point (-1) 5) Up))
    
  , ("Snake with negative y should be dead", 
     test "negative y" $ not $ snakeAlive board10x10 (singlePointSnake (Point 5 (-1)) Up))
    
    -- Test snake out of bounds - too large coordinates
  , ("Snake with x >= xdim should be dead", 
     test "x too large" $ not $ snakeAlive board10x10 (singlePointSnake (Point 10 5) Up))
    
  , ("Snake with y >= ydim should be dead", 
     test "y too large" $ not $ snakeAlive board10x10 (singlePointSnake (Point 5 10) Up))
    
    -- Test snake with body parts out of bounds
  , ("Multi-segment snake with head in bounds but tail out should be dead", 
     test "tail out" $ not $ snakeAlive board5x5 (multiPointSnake [Point 2 2, Point 2 1, Point 2 0, Point 2 (-1)] Up))
    
    -- Test self-collision
  , ("Snake with no self-collision should be alive", 
     test "no collision" $ snakeAlive board10x10 (multiPointSnake [Point 2 2, Point 2 1, Point 2 0] Up))
    
  , ("Snake with self-collision should be dead", 
     test "self collision" $ not $ snakeAlive board10x10 (multiPointSnake [Point 2 2, Point 2 1, Point 2 2] Up))
    
  , ("Snake with duplicate body parts should be dead", 
     test "duplicate parts" $ not $ snakeAlive board10x10 (multiPointSnake [Point 1 1, Point 1 2, Point 1 1, Point 1 3] Up))
    
    -- Edge cases
  , ("Single point snake at corner should be alive", 
     test "corner" $ snakeAlive board5x5 (singlePointSnake (Point 4 4) Down))
    
  , ("Long snake in bounds should be alive", 
     test "long snake" $ snakeAlive board10x10 (multiPointSnake [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 0 4] Up))
  ]

-- Test cases for moveSnake function
testMoveSnake :: [(String, TestResult)]
testMoveSnake = 
  [ -- Test basic movement in all directions
    ("Snake moving right should move head right", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 5 5 L.:| [Point 4 5], direction = Right }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (newSnake, _, _) = moveSnake gen snake board
         newHead = L.head (body newSnake)
     in test "move right" $ newHead == Point 6 5)
    
  , ("Snake moving left should move head left", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 5 5 L.:| [Point 6 5], direction = Left }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (newSnake, _, _) = moveSnake gen snake board
         newHead = L.head (body newSnake)
     in test "move left" $ newHead == Point 4 5)
    
  , ("Snake moving up should move head up", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 5 5 L.:| [Point 5 6], direction = Up }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (newSnake, _, _) = moveSnake gen snake board
         newHead = L.head (body newSnake)
     in test "move up" $ newHead == Point 5 4)
    
  , ("Snake moving down should move head down", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 5 5 L.:| [Point 5 4], direction = Down }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (newSnake, _, _) = moveSnake gen snake board
         newHead = L.head (body newSnake)
     in test "move down" $ newHead == Point 5 6)
    
    -- Test normal movement (no fruit)
  , ("Snake should lose tail when not eating fruit", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 2 2 L.:| [Point 1 2, Point 0 2], direction = Right }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (newSnake, _, _) = moveSnake gen snake board
         expectedBody = [Point 3 2, Point 2 2, Point 1 2]
     in test "lose tail" $ L.toList (body newSnake) == expectedBody)
    
    -- Test eating fruit
  , ("Snake should grow when eating fruit", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 2 2 L.:| [Point 1 2], direction = Right }
         board = Board { xdim = 10, ydim = 10, fruit = Point 3 2 }  -- Fruit at next position
         (newSnake, _, _) = moveSnake gen snake board
         expectedBody = [Point 3 2, Point 2 2, Point 1 2]  -- Snake grows
     in test "grow when eating" $ L.toList (body newSnake) == expectedBody)
    
  , ("Board should get new fruit when snake eats fruit", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 2 2 L.:| [Point 1 2], direction = Right }
         board = Board { xdim = 10, ydim = 10, fruit = Point 3 2 }
         (_, newBoard, _) = moveSnake gen snake board
     in test "new fruit generated" $ fruit newBoard /= Point 3 2)
    
    -- Test single segment snake
  , ("Single segment snake should move correctly", 
     let gen = mkStdGen 42
         snake = Snake { body = L.singleton (Point 5 5), direction = Up }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (newSnake, _, _) = moveSnake gen snake board
     in test "single segment move" $ 
        L.toList (body newSnake) == [Point 5 4] && L.length (body newSnake) == 1)
    
  , ("Single segment snake eating fruit should grow", 
     let gen = mkStdGen 42
         snake = Snake { body = L.singleton (Point 5 5), direction = Up }
         board = Board { xdim = 10, ydim = 10, fruit = Point 5 4 }  -- Fruit at next position
         (newSnake, _, _) = moveSnake gen snake board
     in test "single segment grow" $ 
        L.toList (body newSnake) == [Point 5 4, Point 5 5] && L.length (body newSnake) == 2)
    
    -- Test that board stays same when no fruit eaten
  , ("Board should stay same when no fruit eaten", 
     let gen = mkStdGen 42
         snake = Snake { body = Point 2 2 L.:| [Point 1 2], direction = Right }
         board = Board { xdim = 10, ydim = 10, fruit = Point 8 8 }
         (_, newBoard, _) = moveSnake gen snake board
     in test "board unchanged" $ newBoard == board)
  ]

main :: IO ()
main = do
  runTests testSnakeAlive
  putStrLn ""
  runTests testMoveSnake
