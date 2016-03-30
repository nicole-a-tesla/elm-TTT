module TestRunner (..) where

import Tests exposing (..)
import ElmTest exposing (..)


main =
  elementRunner
    <| suite
        "Tic Tac Toe Tests"
        [ Tests.testSuite
        ]
