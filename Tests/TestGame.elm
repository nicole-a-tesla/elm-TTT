module TestGame (..) where

import ElmTest exposing (..)
import Game exposing (..)
import DataTypes exposing (..)
import BoardHelpers exposing (..)

gameTests : Test
gameTests =
  suite
    "Test for game functions"
    [ test
        "== tests for equality"
        (assert (2 == 2))
    , test
        "Test that startgame winner is Empty"
        (assertEqual startGame.winner Empty)
    , test
        "Test that startgame has a new blank board"
        (assertEqual startGame.board [  [Empty, Empty, Empty]
                                      , [Empty, Empty, Empty]
                                      , [Empty, Empty, Empty]
                                      ])
    , test
        "Test that Game updates board on Move action"
        (assertEqual [[X, Empty, Empty],
                      [Empty, Empty, Empty]
                     ,[Empty, Empty, Empty]]
                     (Game.update (Move 0 0) testGame).board )
    , test
        "Test that Game updates winner on winning move"
        (assertEqual X (Game.update (Move 0 0) nextMoveWinsGame).winner)
    , test
        "Test that Game does not update on NoOp action"
        (assertEqual empty3x3Board (Game.update (NoOp) testGame).board)
    ]
