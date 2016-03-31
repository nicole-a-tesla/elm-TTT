module Board where
import Dict exposing (..)

type Cell
  = X
  | O
  | Empty

type Player
  = Human
  | Computer

newBoard : Dict number Cell
newBoard =
  fromList ([ (1,Empty), (2,Empty), (3,Empty)
            , (4,Empty), (5,Empty), (6,Empty)
            , (7,Empty), (8,Empty), (9,Empty)
            ] )

getSymbol : Player -> Cell
getSymbol player =
  case player of
    Human -> X
    Computer -> O

update : Dict comparable Cell -> comparable -> Player -> Dict comparable Cell
update board index player =
  Dict.insert index (getSymbol player) board
