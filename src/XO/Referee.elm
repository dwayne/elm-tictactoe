module XO.Referee exposing (Outcome (..), Location (..), unsafeDecide)


import Lib exposing (lookup)
import XO.Board as Board exposing (Board, Tile)
import XO.Mark exposing (Mark)


type Outcome
  = Win Location
  | Squash

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
      Just <| Win location

    Nothing ->
      if isSquash tiles then
        Just Squash
      else
        Nothing


findWin : List Tile -> Mark -> Maybe Location
findWin tiles mark =
  let
    t =
      Just mark
  in
  lookup (t, t, t) (arrangements tiles)


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


isSquash : List Tile -> Bool
isSquash = List.all <| (/=) Nothing
