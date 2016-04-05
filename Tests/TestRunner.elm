module Main (..) where

import Console
import ElmTest exposing (..)
import Signal exposing (Signal)
import Task
import TestGroup


port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner TestGroup.tests)
