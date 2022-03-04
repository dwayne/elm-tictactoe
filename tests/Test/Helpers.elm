module Test.Helpers exposing (putMany, playMany)


import XO.Board as Board exposing (Board, Position)
import XO.Game as Game exposing (Game)
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


playMany : Mark -> List Position -> Game
playMany mark positions =
  playManyHelper positions <| Game.start mark


playManyHelper : List Position -> Game -> Game
playManyHelper positions game =
  case positions of
    [] ->
      game

    p :: rest ->
      case Game.play p game of
        Ok newGame ->
          playManyHelper rest newGame

        Err _ ->
          playManyHelper rest game
