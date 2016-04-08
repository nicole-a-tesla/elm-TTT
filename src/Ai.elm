module Ai (..) where

import DataTypes exposing (..)
import Board exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import List  exposing (..)

computerMarker : Cell
computerMarker = DataTypes.O

opponentMarker : Cell
opponentMarker = DataTypes.X

scoreBoard : List (List Cell) -> number
scoreBoard board =
  if (checkWinner board == computerMarker)
    then 10
  else if (checkWinner board == opponentMarker)
    then-10
  else 0

getMinOrMax : Dict comparable number -> Cell -> Maybe number
getMinOrMax scores currentMarker =
  if currentMarker == computerMarker
    then getValuesKey scores (getMaxValue scores)
  else getValuesKey scores (getMinValue scores)

getValuesKey : Dict comparable comparable -> Maybe comparable -> Maybe comparable
getValuesKey keysAndVals val =
  Dict.filter (\k v -> v == (Maybe.withDefault 100 val)) keysAndVals |> Dict.keys |> List.head

getMaxValue : Dict comparable comparable -> Maybe comparable
getMaxValue scores =
  scores |> Dict.values |> List.maximum

getMinValue : Dict comparable comparable -> Maybe comparable
getMinValue scores =
  scores |> Dict.values |> List.minimum

minimaxMove : List (List Cell) -> number
minimaxMove board =
  -- TEMP VALUE
  8

flattenBoard : List (List Cell) -> List Cell
flattenBoard board =
  List.concat(board)

getEmptySpaces board =
  let
    indexedEmptyCells = flattenBoard(board) |> indexedElements |> List.filter (\elem -> (snd elem == Empty))
  in
    indexedEmptyCells |> List.unzip |> fst

indexedElements : List a -> List (Int, a)
indexedElements inputList =
  List.indexedMap (,) inputList
