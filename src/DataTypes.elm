module DataTypes where

type alias Game =
  { board: List Cell }

type Cell
  = X
  | O
  | Empty

type Player
  = Human
  | Computer

type Action = NoOp | Move Int
