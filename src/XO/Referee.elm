module XO.Referee exposing
  ( Outcome(..)
  , Location(..)
  , unsafeDecide
  , isWinningPosition
  )


import AList
import XO.Board as Board exposing (Board, Position, Tile)
import XO.Mark exposing (Mark)


type Outcome
  = Win Mark Location
  | Draw Mark

type Location
  = R1
  | R2
  | R3
  | C1
  | C2
  | C3
  | D1
  | D2


unsafeDecide : Board -> Mark -> Maybe Outcome
unsafeDecide board mark =
  let
    tiles =
      Board.tiles board
  in
  case findWin tiles mark of
    Just location ->
      Just (Win mark location)

    Nothing ->
      if isDraw tiles then
        Just (Draw mark)
      else
        Nothing


findWin : List Tile -> Mark -> Maybe Location
findWin tiles mark =
  let
    t =
      Just mark
  in
  AList.lookup (t, t, t) (arrangements tiles)


arrangements : List Tile -> List ((Tile, Tile, Tile), Location)
arrangements tiles =
  case tiles of
    [a, b, c, d, e, f, g, h, i] ->
      [ ((a, b, c), R1)
      , ((d, e, f), R2)
      , ((g, h, i), R3)
      , ((a, d, g), C1)
      , ((b, e, h), C2)
      , ((c, f, i), C3)
      , ((a, e, i), D1)
      , ((c, e, g), D2)
      ]

    _ ->
      []


isDraw : List Tile -> Bool
isDraw = List.all ((/=) Nothing)


isWinningPosition : Position -> Outcome -> Bool
isWinningPosition (r, c) outcome =
  case outcome of
    Win _ location ->
      case location of
        R1 ->
          r == 0

        R2 ->
          r == 1

        R3 ->
          r == 2

        C1 ->
          c == 0

        C2 ->
          c == 1

        C3 ->
          c == 2

        D1 ->
          r == c

        D2 ->
          r + c == 2

    _ ->
      False
