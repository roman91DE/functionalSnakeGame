{-# OPTIONS_GHC -Wno-unused-imports #-}

module MyLib where

import qualified Data.List.NonEmpty as L
import System.Random (StdGen, mkStdGen, randomR)
import Prelude hiding (Left, Right)

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

data Direction = Left | Right | Up | Down
  deriving (Show, Eq)

data Snake = Snake
  { body :: L.NonEmpty Point,
    direction :: Direction
  }
  deriving (Show, Eq)

data Board = Board
  { xdim :: Int,
    ydim :: Int,
    fruit :: Point
  }
  deriving (Show, Eq)

snakeAlive :: Board -> Snake -> Bool
snakeAlive board snake = not (hitWall || bitItSelf)
  where
    hitWall =
      let f p = (x p < 0 || x p >= xdim board) || (y p < 0 || y p >= ydim board)
       in any f (body snake)

    bitItSelf =
      let countDeduped = length $ L.group $ L.sort (body snake)
       in L.length (body snake) /= countDeduped

moveSnake :: StdGen -> Snake -> Board -> (Snake, Board, StdGen)
moveSnake gen snake board = (newSnake, newBoard, finalGen)
  where
    headPos = L.head $ body snake
    newPos = case direction snake of
      Left -> headPos {x = x headPos - 1}
      Right -> headPos {x = x headPos + 1}
      Up -> headPos {y = y headPos - 1}
      Down -> headPos {y = y headPos + 1}
    hasFruit = fruit board == newPos
    newBody
      | hasFruit = newPos `L.cons` body snake
      | otherwise = case L.init (body snake) of
          [] -> L.singleton newPos
          xs -> newPos `L.cons` L.fromList xs
    newSnake = snake {body = newBody}
    (newBoard, finalGen)
      | hasFruit = generateFruit gen board newSnake
      | otherwise = (board, gen)

generateFruit :: StdGen -> Board -> Snake -> (Board, StdGen)
generateFruit gen board snake = (board {fruit = newPos}, newGen)
  where
    (newPos, newGen) = generateValidFruit gen board snake

generateValidFruit :: StdGen -> Board -> Snake -> (Point, StdGen)
generateValidFruit gen board snake =
  let (x', gen1) = randomR (0, xdim board - 1) gen
      (y', gen2) = randomR (0, ydim board - 1) gen1
      newPos = Point x' y'
   in if newPos `elem` L.toList (body snake)
        then generateValidFruit gen2 board snake
        else (newPos, gen2)
