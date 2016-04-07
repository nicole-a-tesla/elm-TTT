module Ai (..) where

import DataTypes exposing (..)
import Board exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import List  exposing (..)

scoreBoard : List (List Cell) -> number

scoreBoard board =
  if (checkWinner board == DataTypes.O)
    then 10
  else if (checkWinner board == DataTypes.X)
    then-10
  else 0

getMinOrMax : Dict comparable number -> Cell -> Maybe number
getMinOrMax scores currentMarker =
  if (currentMarker == DataTypes.O)
    then (getValuesKey scores (getMaxValue scores))
  else (getValuesKey scores (getMinValue scores))

getValuesKey : Dict comparable comparable -> Maybe comparable -> Maybe comparable
getValuesKey keysAndVals val =
  List.head <| Dict.keys <| Dict.filter (\k v -> v == (Maybe.withDefault 100 val)) keysAndVals

getMaxValue : Dict comparable comparable -> Maybe comparable
getMaxValue scores =
  List.maximum <| Dict.values <| scores

getMinValue : Dict comparable comparable -> Maybe comparable
getMinValue scores =
  List.minimum <| Dict.values <| scores

minimaxMove : List (List Cell) -> number
minimaxMove board =
  -- TEMP VALUE
  8

flattenBoard : List (List Cell) -> List Cell
flattenBoard board =
  List.concat(board)

getEmptySpaces flatBoard =
  let
    indexed = indexedTuples(flatBoard)
    filtered = List.filter (\elem -> (snd elem == Empty)) indexed
    unzipped = List.unzip(filtered)
  in
  fst unzipped

indexedTuples : List a -> List (Int, a)
indexedTuples inputList =
  List.indexedMap (,) inputList
