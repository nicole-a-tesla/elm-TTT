module DataTypes where

type alias Game =
  {
    board: List (List Cell),
    winner: Cell
  }

boardSize = 3

type Cell
  = X
  | O
  | Empty

type Player
  = Human
  | Computer

type Action = NoOp | Move Int Int
