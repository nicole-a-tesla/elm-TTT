module Tests (..) where

import ElmTest exposing (..)
import Board exposing (..)
import Game exposing (..)
import DataTypes exposing(..)

emptyBoard : List Cell
emptyBoard =
  [ Empty, Empty, Empty]

testGame : Game
testGame =
  {board = emptyBoard}

boardTests : Test
boardTests =
  suite
    "Tests for board functions"
    [ test
        "== tests for equality"
        (assert (1 == 1))
    , test
        "New Board is Empty"
        (assertEqual Board.newBoard [ Empty, Empty, Empty
                                    , Empty, Empty, Empty
                                    , Empty, Empty, Empty
                                    ] )
    , test
      "Return X for Human"
      (assertEqual (Board.getSymbol Human) X)
    , test
      "Return O for Computer"
      (assertEqual (Board.getSymbol Computer) O)
    , test
      "Update board with x in 1"
      (assertEqual (Board.update [ Empty, Empty] 0 Human)[ X, Empty])
    ]
gameTests : Test
gameTests =
  suite
    "Test for game functions"
    [ test
        "== tests for equality"
        (assert (2 == 2))
    , test
        "Test that startgame has a new blank board"
        (assertEqual startGame.board [ Empty, Empty, Empty
                                      , Empty, Empty, Empty
                                      , Empty, Empty, Empty
                                      ])
    , test
        "Test that Game updates on Move action"
        (assertEqual (Game.update (Move 0) testGame).board [X, Empty, Empty])
    , test
        "Test that Game does not updates on NoOp action"
        (assertEqual (Game.update (NoOp) testGame).board [Empty, Empty, Empty])

    ]
