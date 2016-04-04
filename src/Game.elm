module Game (..) where

import Html exposing (Html)
import Signal

import Board exposing (..)
import DataTypes exposing (..)
import Display exposing (..)
import Mailbox exposing (..)

startGame : Game
startGame =
  {
    board = newBoard
  }

update : Action -> Game -> Game
update action game =
  case action of
    NoOp -> game
    Move id ->
      { game | board = Board.update game.board id Human }

model : Signal Game
model =
  Signal.foldp update startGame actions.signal

main : Signal Html
main =
  Signal.map (view actions.address) model

