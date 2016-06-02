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
        "Test that startgame winner is Empty"
        (assertEqual startGame.winner Empty)

    , test
        "Test that startgame has a new blank board"
        (assertEqual startGame.board testBoardEmpty)

    , test
        "Test that state is correctly updated on humanPlayerMove"
        (assertEqual (getXActiveStateFor testBoardA)
                     (humanPlayerMove (getXActiveStateFor testBoardEmpty) 0 0))

    , test
        "Test that winner is updated on winning move"
        (assertEqual X (humanPlayerMove (getXActiveStateFor test3x3Board) 0 0).winner)

    , test
        "Test that Game does not update on NoOp action"
        (assertEqual empty3x3Board (Game.update (NoOp) (getOActiveStateFor empty3x3Board)).board)
    ]

testBoardEmpty : List (List Cell)
testBoardEmpty = 
     [[Empty, Empty, Empty],
     [Empty, Empty, Empty],
     [Empty, Empty, Empty]]

testBoardA : List (List Cell)
testBoardA =
    [[X, Empty, Empty], 
     [Empty, Empty, Empty],
     [Empty, Empty, Empty]]

