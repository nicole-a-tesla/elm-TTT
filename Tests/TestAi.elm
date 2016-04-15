module TestAi (..) where

import ElmTest exposing (..)
import Ai exposing (..)
import DataTypes exposing (..)
import BoardHelpers exposing (..)
import Dict exposing (..)
import Array exposing (..)

aiInfrastructureTest : Test
aiInfrastructureTest =
  suite
  "Test Ai Infrastructure"
  [ test
      "getScore returns 10 for current marker win"
      (assertEqual 10 (getScore oWins3x3Board))

  , test
    "getScore returns -10 for opponent marker win"
    (assertEqual -10 (getScore winner3x3Board))

  , test
    "getScore returns 0 for non-winning board"
    (assertEqual 0 (getScore test3x3Board))

  , test
    "selects max score if currentMarker is computer marker"
    (assertEqual 2
      (getMinOrMaxIndex oTurnGameState (Array.fromList [0, 1, 2])))

  , test
    "selects min score cell if currentMarker is opponent marker"
    (assertEqual 0
      (getMinOrMaxIndex xTurnGameState (Array.fromList [0, 1, 2])))

  , test
    "getMaxValue gets max"
    (assertEqual 10
      (getMaxValue (Array.fromList [0, 5, 10])))

  , test
    "getMinValue gets max"
    (assertEqual 0
      (getMinValue (Array.fromList [0, 5, 10])))

  , test
    "emptySpaces returns empty spaces - nearly full board"
    (assertEqual [{x=2, y=2}]
      (getEmptySpacesInBoard(oneEmptySpace)))

  , test
    "emptySpaces returns empty spaces - nearly empty board"
    (assertEqual [{x=0, y=0}, {x=0, y=1},
                  {x=1, y=0}, {x=1, y=1},
                  {x=2, y=0}, {x=2, y=1}, {x=2, y=2}]
      (getEmptySpacesInBoard(nearWin)))

  , test
    "emptySpacesRow returns empty spaces"
    (assertEqual [{x=0, y=1}, {x=0, y=2}]
      (getEmptySpacesInRow((0, [X, Empty, Empty]))))

  , test
    "fromJust converts numbers"
    (assertEqual 0
      (fromJust (Just 0)))

  , test
    "fromJust converts tuples"
    (assertEqual (0,0)
      (fromJust (Just (0,0))))

  , test
    "converts tuple to coordset"
    (assertEqual {x=1, y=2}
      (toCoordSet (1,2)))

  , test
    "get index returns middle index"
    (assertEqual 1
      (getIndexOf 100 (Array.fromList([10, 100, 0]))))

  , test
    "get index returns first index if multi matches"
    (assertEqual 0
      (getIndexOf 100 (Array.fromList([100, 100, 0]))))

  , test
    "translates coord to position"
    (assertEqual (0//1)
      (toFlatIndex {x=0, y=0}))

  , test
    "translates coord to position"
    (assertEqual (1//1)
      (toFlatIndex {x=0, y=1}))

  , test
    "translates coord to position"
    (assertEqual (2//1)
      (toFlatIndex {x=0, y=2}))

  , test
    "translates coord to position"
    (assertEqual (3//1)
      (toFlatIndex {x=1, y=0}))

  , test
    "translates coord to position"
    (assertEqual (4//1)
      (toFlatIndex {x=1, y=1}))

  , test
    "translates coord to position"
    (assertEqual (5//1)
      (toFlatIndex {x=1, y=2}))

  , test
    "translates coord to position"
    (assertEqual (6//1)
      (toFlatIndex {x=2, y=0}))

  , test
    "translates coord to position"
    (assertEqual (7//1)
      (toFlatIndex {x=2, y=1}))

  , test
    "translates coord to position"
    (assertEqual (8//1)
      (toFlatIndex {x=2, y=2}))
  ]

boardA : List (List Cell)
boardA =
  [ [X, O, X]
  , [O, O, Empty]
  , [O, X, X]
  ]

gameAState : GameState
gameAState =
  { board = boardA,
    activePlayer = X,
    inactivePlayer = O }

boardB : List (List Cell)
boardB =
  [ [X, O, X]
  , [O, O, X]
  , [O, X, X]
  ]

gameBState : GameState
gameBState =
  { board = boardB,
    activePlayer = O,
    inactivePlayer = X }

minimaxTest : Test
minimaxTest =
  suite
  "Test Minimax & Whole-Board-Scoring"
  [
    test
    "nextState correct board"
    (assertEqual gameBState
      (nextState gameAState {x=1, y=2}))

  , test
    "chooses winning move from one choice"
    (assertEqual 8
      (minimaxMove oneEmptySpaceState))

  , test
    "chooses winning move from board with many empty spaces"
    (assertEqual 8
      (minimaxMove nearWinGameState))

  , test
    "selects losing move if that's all there is"
    (assertEqual 0
      (minimaxMove xWinsOnZeroState))

  , test
    "blocks opponent if no winning move"
    (assertEqual 2
      (minimaxMove blockingBoardGameState))

  , test
    "takes winning move over block if available"
    (assertEqual 8
      (minimaxMove blockOrWinGameState))

  ]

oTurnGameState : GameState
oTurnGameState =
  { board = blockingBoard,
    activePlayer = O,
    inactivePlayer = X }

xTurnGameState : GameState
xTurnGameState =
  { board = blockingBoard,
    activePlayer = X,
    inactivePlayer = O }

blockOrWin : List (List Cell)
blockOrWin =
  [ [X,     X,     Empty]
  , [Empty, Empty, Empty]
  , [O,     O,     Empty]
  ]

blockOrWinGameState =
  {board = blockOrWin,
   activePlayer = O,
   inactivePlayer = X}

blockingBoard : List (List Cell)
blockingBoard =
  [ [X,     X,     Empty]
  , [Empty, Empty, Empty]
  , [O,     Empty, Empty]
  ]

blockingBoardGameState =
  {board = blockingBoard,
   activePlayer = O,
   inactivePlayer = X}

threeEmptyOneWin : List (List Cell)
threeEmptyOneWin =
  [ [X, Empty,     O]
  , [O, Empty, Empty]
  , [O, X,         X]
  ]

threeEmptyOneWinGameState =
  {board = threeEmptyOneWin,
   activePlayer = O,
   inactivePlayer = X}

threeEmptyOneWinExpected =
  Dict.insert (0//1, 1//1) 0
    (Dict.insert (1//1, 1//1) 10
      (Dict.insert (1//1, 2//1) 0
        Dict.empty))

nearWinGameState =
  { board = nearWin,
    activePlayer = O,
    inactivePlayer = X }

oneEmptySpaceState =
  { board = oneEmptySpace,
    activePlayer = O,
    inactivePlayer = X }

multipleChoice : List (List Cell)
multipleChoice =
  [ [    X, X, O]
  , [Empty, O, O]
  , [Empty, X, X]
  ]

multipleChoiceState =
  { board = multipleChoice,
    activePlayer = O,
    inactivePlayer = X }

nearWinExpectedScores =
  Dict.insert (0//1, 0//1) 0
    (Dict.insert (0//1, 1//1) 0
      (Dict.insert (1//1, 0//1) 0
        (Dict.insert (1//1, 1//1) 0
          (Dict.insert (2//1, 0//1) 0
            (Dict.insert (2//1, 1//1) 0
              (Dict.insert (2//1, 2//1) 10
      Dict.empty))))))


xWinsOnZero : List (List Cell)
xWinsOnZero =
  [ [Empty, X, O]
  , [    X, X, O]
  , [    O, O, X]
  ]

xWinsOnZeroState =
  { board = xWinsOnZero,
    activePlayer = X,
    inactivePlayer = O }

xWinsOnZeroExpectedScore =
  Dict.insert (0 // 1, 0 // 1) -10 Dict.empty

oneEmptySpaceExpectedScore =
  Dict.insert (2 // 1, 2 // 1) 10 Dict.empty

testScores =
  let
    stepOne = Dict.insert (0,0) 10 Dict.empty
    stepTwo = Dict.insert (0,1) -10 stepOne
  in
    Dict.insert (0,2) 0 stepTwo
