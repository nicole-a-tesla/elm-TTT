module Main (..) where

import Console
import ElmTest exposing (..)
import Signal exposing (Signal)
import Task
import Tests


tests : Test
tests =
  suite
    "Tic Tac Toe Tests"
    [ Tests.testSuite
    ]

port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner tests)
