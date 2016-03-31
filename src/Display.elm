module Display (..) where

import Html exposing (Html, div, h1)
import Mouse


view : Html
view =
  h1 [] [ Html.text "Welcome to Tic Tac Toe"]

main =
  view
