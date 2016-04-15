module Ai (..) where

import DataTypes exposing (..)
import Board exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import List exposing (..)
import Array exposing (..)

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

nextState: GameState -> Coords -> GameState
nextState gameState move =
  {board = Board.update gameState move,
   activePlayer = gameState.inactivePlayer,
   inactivePlayer = gameState.activePlayer}

minimaxMove : GameState -> Int
minimaxMove gameState =
  if (Board.checkWinner gameState.board) /= Empty then
    getScore gameState.board
  else

    let
      moves      = Array.fromList <| getEmptySpacesInBoard gameState.board
      nextStates = Array.map (\move -> nextState gameState move) moves
      scores     = Array.map (\possibleState -> minimaxMove possibleState) nextStates

      chosenMove = Array.get (getMinOrMaxIndex gameState scores) moves
    in
      toFlatIndex (fromJust chosenMove)

toFlatIndex : Coords -> Int
toFlatIndex coord =
  if coord.x == 0 then
    coord.y
  else if coord.x == 1 then
    coord.y + coord.x + 2
  else if coord.x == 2 then
    coord.x + coord.y + 4
  else
    Debug.crash "error: Out of bounds index"

getIndexOf : a -> Array a -> Int
getIndexOf elem coll =
  let
    indexedCollection = List.indexedMap (,) (Array.toList coll)
    searchResults = List.filter (\pair -> (snd pair) == elem) indexedCollection
    unwrappedResult = fromJust <| List.head searchResults
  in
    fst unwrappedResult

getMinOrMaxIndex : GameState -> Array Int -> Int
getMinOrMaxIndex gameState scores =
  if gameState.activePlayer == computerMarker then
    getIndexOf (scores |> getMaxValue) scores
  else
    getIndexOf (scores |> getMinValue) scores

getMaxValue : Array comparable -> Int
getMaxValue scores =
  fromJust <| List.maximum <| Array.toList scores

getMinValue : Array comparable -> Int
getMinValue scores =
  fromJust <| List.minimum <| Array.toList scores

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

fromJust : Maybe a -> a
fromJust x =
  case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

toCoordSet : (Int, Int) -> Coords
toCoordSet tup =
  {x=(fst tup), y=(snd tup)}
