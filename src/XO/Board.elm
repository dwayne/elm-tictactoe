module XO.Board exposing
  ( Board, Position
  , empty
  , put
  , isOpen, inBounds, openPositions
  , Tile, tiles
  , Cell, cells
  , toString
  )


import AList
import XO.Mark as Mark exposing (Mark(..))


type Board = Board (List Move)

type alias Move = (Position, Mark)
type alias Position = (Int, Int)


-- CONSTRUCT


empty : Board
empty = Board []


-- MODIFY


put : Position -> Mark -> Board -> Board
put p mark (Board moves) = Board ((p, mark) :: moves)


-- QUERY


isOpen : Position -> Board -> Bool
isOpen p (Board moves) = isNotMember p moves


inBounds : Position -> Bool
inBounds (r, c) = r >= 0 && r < 3 && c >= 0 && c < 3


openPositions : Board -> List Position
openPositions (Board moves) = List.filter (flip isNotMember moves) allPositions


-- CONVERT


type alias Tile = Maybe Mark


tiles : Board -> List Tile
tiles (Board moves) = List.map (flip AList.lookup moves) allPositions


type alias Cell = (Position, Tile)


cells : Board -> List Cell
cells board = zip allPositions (tiles board)


toString : Board -> String
toString board =
  tiles board
    |> List.map (Maybe.withDefault "." << Maybe.map Mark.toString)
    |> String.concat


-- CONSTANTS


allPositions : List Position
allPositions =
  [ (0, 0), (0, 1), (0, 2)
  , (1, 0), (1, 1), (1, 2)
  , (2, 0), (2, 1), (2, 2)
  ]


-- HELPERS


isNotMember : a -> List (a, b) -> Bool
isNotMember key = (==) Nothing << AList.lookup key


flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b


zip : List a -> List b -> List (a, b)
zip xs ys =
  case (xs, ys) of
    ([], _) ->
      []

    (_, []) ->
      []

    (x :: xRest, y :: yRest) ->
      (x, y) :: zip xRest yRest
