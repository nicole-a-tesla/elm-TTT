module Tests (..) where

import ElmTest exposing (..)
import Board exposing (..)
import Game exposing (..)
import DataTypes exposing(..)
import Display exposing (..)

emptyBoard : List (List Cell)
emptyBoard =
  [ [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  ]

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
        (assertEqual Board.newBoard [ [Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] )
    , test
      "Return X for Human"
      (assertEqual (Board.getSymbol Human) X)
    , test
      "Return O for Computer"
      (assertEqual (Board.getSymbol Computer) O)
    , test
      "Extract item from list"
      (assertEqual  (Just [1,2,3]) (Board.extractFromList [[1,2,3],[4,5,6]] 0))
    , test
    "Extract another item from list"
    (assertEqual (Just 1)(Board.extractFromList [1,2,3] 0))
    , test
    "set nth item in list"
    (assertEqual ([X, Empty])(Board.setNthItem [Empty, Empty] 0 X))
    , test
      "set nth item in list of lists"
      (assertEqual ([[X, Empty], [Empty, Empty]])(Board.setNthItem [[Empty, Empty], [Empty, Empty]] 0 [X, Empty]))
    , test
      "Get Row from Board"
      (assertEqual ([Empty, Empty, Empty])(Board.getRow emptyBoard 0))
    , test
      "Update board with x in 1"
      (assertEqual [[ X, Empty], [Empty, Empty]] (Board.update [[Empty, Empty],[Empty, Empty]] 0 0 Human))
    , test
      "Check Row for X Winner"
      (assertEqual "X Wins" (Board.checkListForWin [X, X, X]))
    , test
      "Check Row for O Winner"
      (assertEqual "O Wins" (Board.checkListForWin [O, O, O]))
    , test
      "Check Row for No Winner"
      (assertEqual "No Winner" (Board.checkListForWin [O, Empty, O]))
    , test
      "Check for 2 Unique Sybols"
      (assertEqual 2 (Board.checkNumberOfUniqueSymbols [X, O, X]))

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
        (assertEqual startGame.board [ [Empty, Empty, Empty]
                                      , [Empty, Empty, Empty]
                                      , [Empty, Empty, Empty]
                                      ])
    , test
        "Test that Game updates on Move action"
        (assertEqual (Game.update (Move 0 0) testGame).board [[X, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]])
    , test
        "Test that Game does not update on NoOp action"
        (assertEqual emptyBoard (Game.update (NoOp) testGame).board)
    ]
displayTest : Test
displayTest =
  suite
    "Test for behavioral changes to display functions"
    [ test
        "Test converting Empty Cell to String"
        (assertEqual "5" (convertCellToString 4 Empty))
    , test
        "Test converting X Cell to String"
        (assertEqual (convertCellToString 0 X) "X")
    , test
        "Test converting O Cell to String"
        (assertEqual (convertCellToString 0 O) "O")
    ]
