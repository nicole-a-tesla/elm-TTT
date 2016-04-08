module Game (..) where

import Html exposing (Html)
import Signal

import Board exposing (..)
import DataTypes exposing (..)
import Display exposing (..)
import Mailbox exposing (..)
import Debug

startGame : Game
startGame =
  {
    board = newBoard,
    winner = Empty
  }
updateGameBoard : Game -> Int -> Int -> Game
updateGameBoard game row column =
  { game | board = Board.update game.board row column X}

updateWinnerStatus : Game -> Game
updateWinnerStatus game  =
  {game | winner = checkWinner game.board}

update : Action -> Game -> Game
update action game =
  case action of
    NoOp -> game
    Move row column ->
      updateGameBoard game row column
        |> updateWinnerStatus

model : Signal Game
model =
  Signal.foldp update startGame actions.signal

main : Signal Html
main =
  Signal.map (view actions.address) model

