module Main (..) where

import Console
import ElmTest exposing (..)
import Signal exposing (Signal)
import Task
import Tests


main =
  elementRunner
    <| suite
      "Tic Tac Toe Tests"
      [ Tests.boardTests,
        Tests.gameTests
      ]


