module Board where

import Dict exposing (..)

import DataTypes exposing (..)

newBoard : List Cell
newBoard =
  [ Empty, Empty, Empty
  , Empty, Empty, Empty
  , Empty, Empty, Empty
  ]

getSymbol : Player -> Cell
getSymbol player =
  case player of
    Human -> X
    Computer -> O

update : List Cell -> Int -> Player -> List Cell
update board desiredIndex player =
  List.indexedMap
    (\currentIndex cell ->
      if desiredIndex == currentIndex then (getSymbol player) else cell) board
