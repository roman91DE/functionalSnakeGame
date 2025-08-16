module MyLib where

import qualified Data.List.NonEmpty as L


data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

newtype Snake = Snake
  { body :: L.NonEmpty Point
  }
  deriving (Show, Eq)

data Board = Board
  { xdim :: Int,
    ydim :: Int
  }

snakeAlive :: Board -> Snake -> Bool
snakeAlive board snake = not (hitWall || bitItSelf) 
  where
    hitWall =
      let f p = (x p < 0 || x p >= xdim board) || (y p < 0 || y p >= ydim board) 
       in any f (body snake)

    bitItSelf =
      let countDeduped = length $ L.group $ L.sort (body snake)
       in L.length (body snake) /= countDeduped 
