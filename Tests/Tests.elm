module Tests (..) where

import ElmTest exposing (..)
import Dict exposing (..)
import Board exposing (..)
import Game exposing (..)
import DataTypes exposing(..)
import Display exposing (..)
import Ai exposing (..)

empty3x3Board : List (List Cell)
empty3x3Board =
  [ [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  ]

test3x3Board : List (List Cell)
test3x3Board =
  [ [Empty, X, X]
  , [Empty, O, Empty]
  , [Empty, X, Empty]
  ]

winner3x3Board : List (List Cell)
winner3x3Board =
  [ [X, X, X]
  , [Empty, O, O]
  , [Empty, X, O]
  ]

oWins3x3Board : List (List Cell)
oWins3x3Board =
  [ [X, X, O]
  , [Empty, O, O]
  , [Empty, X, O]
  ]

winner3x3BoardColumns : List (List Cell)
winner3x3BoardColumns =
  [ [X, Empty, Empty]
  , [X, O, X]
  , [X, O, O]
  ]

winner3x3BoardDiagonals : List (List Cell)
winner3x3BoardDiagonals =
  [ [X, O, O]
  , [X, O, Empty]
  ]

all3x3Lists : List (List Cell)
all3x3Lists =
  [ [X, X, X]
  , [Empty, O, O]
  , [Empty, X, O]
  , [X, Empty, Empty]
  , [X, O, X]
  , [X, O, O]
  , [X, O, O]
  , [X, O, Empty]
  ]

empty4x4Board : List (List Cell)
empty4x4Board =
  [ [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  ]

test4x4Board : List (List Cell)
test4x4Board =
  [ [Empty, X, Empty, Empty]
  , [Empty, O, Empty, Empty]
  , [Empty, X, Empty, Empty]
  , [Empty, O, Empty, Empty]
  ]

winner4x4Board : List (List Cell)
winner4x4Board =
  [ [X, X, Empty, Empty]
  , [O, X, Empty, Empty]
  , [O, X, X, Empty]
  , [O, O, O, X]
  ]

winner4x4BoardColumn : List (List Cell)
winner4x4BoardColumn =
  [ [X, O, O, O]
  , [X, X, X, O]
  , [Empty, Empty, X, O]
  , [Empty, Empty, Empty, X]
  ]

winner4x4BoardDiagonals : List (List Cell)
winner4x4BoardDiagonals =
  [ [X, X, X, X]
  , [Empty, Empty, X, O]
  ]

all4x4Lists : List (List Cell)
all4x4Lists =
  [ [X, X, Empty, Empty]
  , [O, X, Empty, Empty]
  , [O, X, X, Empty]
  , [O, O, O, X]
  , [X, O, O, O]
  , [X, X, X, O]
  , [Empty, Empty, X, O]
  , [Empty, Empty, Empty, X]
  , [X, X, X, X]
  , [Empty, Empty, X, O]
  ]

testGame : Game
testGame =
  {
    board = empty3x3Board,
    winner = Empty
  }

nextMoveWinsGame : Game
nextMoveWinsGame =
  {
    board = test3x3Board,
    winner = Empty
  }

testScores : List ( number, number' )
testScores = [ ( 0, 10 ), ( 1, -10 ), (2, 0) ]

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
        "Get Row from 3x3 Board"
        (assertEqual ([Empty, X, X])(Board.getRow test3x3Board 0))
    , test
        "Get Column from 3x3 Board"
        (assertEqual ([X, O, X])(Board.getColumn test3x3Board 1))
    , test
        "Get Diagonal from 3x3 Board"
        (assertEqual ([Empty, O, Empty])(Board.getDiagonal test3x3Board))
    , test
        "Get Anti-Diagonal from 3x3 Board"
        (assertEqual ([X, O, Empty])(Board.getAntiDiagonal test3x3Board))
    , test
        "Get Row from 4x4 Board"
        (assertEqual ([Empty, X, Empty, Empty])(Board.getRow test4x4Board 0))
    , test
        "Get Column from 4x4 Board"
        (assertEqual ([X, O, X, O])(Board.getColumn test4x4Board 1))
    , test
        "Get Diagonal from 4x4 Board"
        (assertEqual ([Empty,O,Empty,Empty])(Board.getDiagonal test4x4Board))
    , test
        "Get Anti-Diagonal from 4x4 Board"
        (assertEqual ([Empty,Empty,X,Empty])(Board.getAntiDiagonal test4x4Board))
    , test
        "Update board with x in 1"
        (assertEqual [[ X, Empty], [Empty, Empty]] (Board.update [[Empty, Empty],[Empty, Empty]] 0 0 Human))
    , test
        "Check Row for X Winner"
        (assertEqual True (Board.checkListForWin [X, X, X]))
    , test
        "Check Row for O Winner"
        (assertEqual True (Board.checkListForWin [O, O, O]))
    , test
        "Check Row for No Winner"
        (assertEqual False (Board.checkListForWin [O, Empty, O]))
    , test
        "Check Row for No Winner on Empty Board"
        (assertEqual False (Board.checkListForWin [Empty, Empty, Empty]))
    , test
        "Check if list is all the same symbol"
        (assertEqual False (Board.listIsUniform [X, O, X]))
    , test
        "Check if list is not all Empty"
        (assertEqual False (Board.empty [X, X, X]))
    , test
        "Check if list is all Empty"
        (assertEqual True (Board.empty [Empty, Empty, Empty]))
    , test
        "Get all 3x3 Columns"
        (assertEqual winner3x3BoardColumns (Board.getAllColumns winner3x3Board))
    , test
        "Get all 4x4 Columns"
        (assertEqual winner4x4BoardColumn (Board.getAllColumns winner4x4Board))
    , test
        "Get both 3x3 Diagonals"
        (assertEqual winner3x3BoardDiagonals (Board.getBothDiagonals winner3x3Board))
    , test
        "Get both 4x4 Diagonals"
        (assertEqual winner4x4BoardDiagonals (Board.getBothDiagonals winner4x4Board))
    , test
        "Gather 3x3 board"
        (assertEqual all3x3Lists (Board.gatherBoardLists winner3x3Board))
    , test
        "Gather 4x4 board"
        (assertEqual all4x4Lists (Board.gatherBoardLists winner4x4Board))
    , test
        "Return Winner"
        (assertEqual X (Board.checkWinner winner4x4Board))
    , test
        "Return Winner"
        (assertEqual X (Board.checkWinner winner3x3Board))
    , test
        "Return Empty if no Winner"
        (assertEqual Empty (Board.checkWinner empty3x3Board))

    ]

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
        (assertEqual startGame.board [ [Empty, Empty, Empty]
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
displayTest : Test
displayTest =
  suite
    "Test for behavioral changes to display functions"
    [ test
        "Test converting Empty Cell to String"
        (assertEqual "5" (convertCellToString 4 Empty))
    , test
        "Test converting X Cell to String"
        (assertEqual "X" (convertCellToString 0 X))
    , test
        "Test converting O Cell to String"
        (assertEqual "O" (convertCellToString 0 O))
    , test
        "Build Winner String X"
        (assertEqual "X Wins" (buildWinnerString X))
    , test
        "Build Winner String O"
        (assertEqual "O Wins" (buildWinnerString O))
    , test
        "Build no Winner String"
        (assertEqual "" (buildWinnerString Empty))
    ]


nearWin : List (List Cell)
nearWin =
  [ [Empty, Empty, O]
  , [Empty, Empty, O]
  , [Empty, Empty, Empty]
  ]

aiTest : Test
aiTest =
  suite
  "Test Ai"
  [ test
      "scoreBoard returns 10 for current marker win"
      (assertEqual 10 (scoreBoard oWins3x3Board))

  , test
    "scoreBoard returns -10 for opponent marker win"
    (assertEqual -10 (scoreBoard winner3x3Board))

  , test
    "scoreBoard returns 0 for non-winning board"
    (assertEqual 0 (scoreBoard test3x3Board))

  , test
    "selects max scoring cell if currentMarker is computer marker"
    (assertEqual (Just 0) (getMinOrMax (Dict.fromList testScores) DataTypes.O))

  , test
    "selects min scoring cell if currentMarker is opponent marker"
    (assertEqual (Just 1) (getMinOrMax (Dict.fromList testScores) DataTypes.X))

  , test
    "getMaxValue gets max"
    (assertEqual (Just 10) (getMaxValue (Dict.fromList testScores)))

  , test
    "getMinValue gets max"
    (assertEqual (Just -10) (getMinValue (Dict.fromList testScores)))

  , test
    "getValuesKey gets the value of its key"
    (assertEqual (Just 0) (getValuesKey (Dict.fromList testScores) (Just 10)))

  , test
    "minimaxMove chooses winning move"
    (assertEqual 8 (minimaxMove nearWin))

  , test
    "emptySpaces returns empty spaces"
    (assertEqual [0, 1, 3, 4, 6, 7, 8] (getEmptySpaces(nearWin)))

  , test
    "flatten-board flattens lists"
    (assertEqual [X, X, O, Empty] (flattenBoard [[X, X], [O, Empty]]))

  , test
    "breaks it up be index"
    (assertEqual [(0, "please"), (1, "no")]
      (indexedElements(["please", "no"])))

  ]
