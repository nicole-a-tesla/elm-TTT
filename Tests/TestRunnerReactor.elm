module Main (..) where

import ElmTest exposing (..)
import Tests


main =
  elementRunner
    <| suite
      "Tic Tac Toe Tests"
      [ Tests.boardTests,
        Tests.gameTests,
        Tests.displayTest
      ]


