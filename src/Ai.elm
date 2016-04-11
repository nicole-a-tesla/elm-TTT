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

getScore : List (List Cell) -> number
getScore board =
  if (checkWinner board == computerMarker)
    then 10
  else if (checkWinner board == opponentMarker)
    then-10
  else 0

scoreWholeBoard : GameState -> Dict (Int, Int) number -> Dict (Int, Int) number
scoreWholeBoard gameState scoresSoFar =
  case getEmptySpacesInBoard(gameState.board) of
    [] ->
      Dict.empty

    [coordSet] ->
      let
        newBoard = Board.update gameState coordSet
      in
        Dict.insert (coordSet.x, coordSet.y) (getScore newBoard) scoresSoFar

    coordSet::coordSets ->
      let
        newScoresSoFar = Dict.insert (coordSet.x, coordSet.y) (getScore gameState.board) scoresSoFar

        newGameState =
          { board = Board.update gameState coordSet,
            activePlayer = gameState.inactivePlayer,
            inactivePlayer = gameState.activePlayer }
      in
        scoreWholeBoard newGameState newScoresSoFar

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

minimaxMove : GameState -> number
minimaxMove gameState =
  -- Temp val
  8

flattenBoard : List (List Cell) -> List Cell
flattenBoard board =
  List.concat(board)

getEmptySpacesInBoard board =
  let
    indexedRows = indexedElements(board)
  in
    concat (List.map (\row -> getEmptySpacesInRow(row)) indexedRows)

getEmptySpacesInRow rowTuple =
  let
    xVal = fst rowTuple
    emptySpaces = indexedElements(snd rowTuple) |> List.filter (\elem -> (snd elem == Empty))
  in
    List.map (\indexedPair -> {x=xVal, y=(fst indexedPair)}) emptySpaces

indexedElements : List a -> List (Int, a)
indexedElements inputList =
  List.indexedMap (,) inputList
