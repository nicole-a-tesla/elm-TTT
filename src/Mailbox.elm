module Mailbox where

import Signal exposing (Signal, Address)

import DataTypes exposing (..)


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp
