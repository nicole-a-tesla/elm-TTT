module Tests (..) where

import ElmTest exposing (..)

testSuite =
  suite
    "About Comparison Operators"
    [ test
        "== tests for equality"
        (assert (1 == 1))
    ]
