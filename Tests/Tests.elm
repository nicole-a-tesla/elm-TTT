module Tests (..) where

import ElmTest exposing (..)
import Board exposing (..)
import Dict exposing (..)


testSuite =
  suite
    "About Comparison Operators"
    [ test
        "== tests for equality"
        (assert (1 == 1))
    , test
        "New Board is (:Empty)"
        (assertEqual Board.newBoard (fromList [ (1,Empty), (2,Empty), (3,Empty)
                                              , (4,Empty), (5,Empty), (6,Empty)
                                              , (7,Empty), (8,Empty), (9,Empty)
                                              ] ))
    , test
      "Return X for Human"
      (assertEqual (Board.getSymbol Human) X)
    , test
      "Return O for Computer"
      (assertEqual (Board.getSymbol Computer) O)
    , test
    "Update board with x in 1"
      (assertEqual (Board.update (Dict.fromList [ (1,Empty), (2,Empty), (3,Empty)
                                                , (4,Empty), (5,Empty), (6,Empty)
                                                , (7,Empty), (8,Empty), (9,Empty)
                                                ] ) 1 Human )
                                  (Dict.fromList [ (1,X), (2,Empty), (3,Empty)
                                                , (4,Empty), (5,Empty), (6,Empty)
                                                , (7,Empty), (8,Empty), (9,Empty)
                                                ] ))
    ]
