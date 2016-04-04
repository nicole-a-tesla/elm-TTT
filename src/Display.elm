module Display (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Signal, Address)

import Board exposing (..)
import DataTypes exposing (..)
import Mailbox exposing (..)


cellStyle : Attribute
cellStyle =
  style
    [("margin", "10px")
    , ("width", "30%")
    , ("height", "200px")
    , ("font-size", "6em")
    , ("border-radius", "50")
    , ("background", "white")
    , ("border", "2px solid #4A4A4A")]

header : Attribute
header =
  style
    [("text-align", "center")]

convertCellToString : Int -> Cell -> String
convertCellToString index cell =
  case cell of
    Empty -> toString (index + 1)
    _ -> toString cell

createCellButton : Int -> Cell -> Html
createCellButton index cell =
  let
    content = convertCellToString index cell
  in
    button [cellStyle, onClick actions.address (Move index)] [text content]

view : Address Action -> Game -> Html
view address game =
  div [] [
    h1 [header] [ text "Welcome to Tic Tac Toe"],
    div [] (List.indexedMap createCellButton game.board)
  ]


