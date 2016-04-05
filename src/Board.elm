module Board where

import DataTypes exposing (..)
import Array exposing (..)
newBoard : List (List Cell)
newBoard =
  [ [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  ]

getSymbol : Player -> Cell
getSymbol player =
  case player of
    Human -> X
    Computer -> O

getRow : List (List Cell) -> Int -> List Cell
getRow board row =
  Maybe.withDefault [] <| get row <| fromList (board)


extractFromList : List a -> Int -> Maybe a
extractFromList list desiredIndex =
  List.head <| List.drop desiredIndex list

setNthItem : List a -> Int  -> a  -> List a
setNthItem list index value=
  toList <| set index value (fromList list)

update : List (List Cell) -> Int -> Int -> Player -> List (List Cell)
update board row column player =
  setNthItem board row (setNthItem (getRow board row) column (getSymbol player))
