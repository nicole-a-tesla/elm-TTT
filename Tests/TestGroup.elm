module TestGroup (..) where

import ElmTest exposing (..)
import Tests

tests : Test
tests =
  suite
    "Tic Tac Toe Tests"
    [ Tests.boardTests,
      Tests.gameTests,
      Tests.displayTest,
      Tests.aiTest
    ]
