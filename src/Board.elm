module Board where

import DataTypes exposing (..)
import Array exposing (..)
import Set
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

checkNumberOfUniqueSymbols : List Cell -> Int
checkNumberOfUniqueSymbols list =
  List.length <| Set.toList <| Set.fromList <| List.map toString list

checkListForWin : List Cell -> String
checkListForWin list =
  if (checkNumberOfUniqueSymbols list) == 1 then
     toString (Maybe.withDefault Empty (List.head list))  ++ " Wins"
  else
    "No Winner"

getRow : List (List Cell) -> Int -> List Cell
getRow board row =
  Maybe.withDefault [] <| get row <| fromList (board)

getColumn : List (List Cell) -> Int -> List Cell
getColumn board column =
 List.map (\list -> Maybe.withDefault Empty <| extractFromList list column) board

getDiagonal : List (List Cell) -> Int -> List Cell
getDiagonal board column =
 List.indexedMap (\index list -> Maybe.withDefault Empty <| extractFromList list index) board

extractFromList : List a -> Int -> Maybe a
extractFromList list desiredIndex =
  List.head <| List.drop desiredIndex list

setNthItem : List a -> Int  -> a  -> List a
setNthItem list index value=
  toList <| set index value (fromList list)

update : List (List Cell) -> Int -> Int -> Player -> List (List Cell)
update board row column player =
  setNthItem board row (setNthItem (getRow board row) column (getSymbol player))
