module Test.Helpers exposing (putMany)


import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark)



putMany : Mark -> List Position -> Board
putMany mark positions =
  putManyHelper mark positions Board.empty


putManyHelper : Mark -> List Position -> Board -> Board
putManyHelper mark positions board =
  case positions of
    [] ->
      board

    p :: rest ->
      putManyHelper (Mark.swap mark) rest (Board.put p mark board)
