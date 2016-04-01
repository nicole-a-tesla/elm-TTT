module Display (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse

cell : Attribute
cell =
  style
    [("margin", "10px")
    , ("width", "30%")
    , ("height", "200px")
    , ("text-align", "center")
    , ("-webkit-user-select", "none")
    , ("user-select", "none")
    , ("font-size", "6em")
    , ("border-radius", "0")
    , ("-webkit-appearance", "none")
    , ("padding", "0px")
    , ("background", "white")
    , ("border", "2px solid #4A4A4A"),
      ("display", "inline-block")]

header : Attribute
header =
  style
    [("text-align", "center")]



view : Html
view =
  div [] [
    h1 [header] [ Html.text "Welcome to Tic Tac Toe"],
    div [cell] [],
    div [cell] [],
    div [cell] [],
    div [cell] [],
    div [cell] [],
    div [cell] [],
    div [cell] [],
    div [cell] [],
    div [cell] []
  ]

main =
  view
