module XO.Board exposing
  ( Board, Position
  , empty
  , put
  , isOpen, inBounds, openPositions
  , Tile
  , tiles
  )


import XO.Mark exposing (Mark)


type Board = Board (List Move)

type alias Move = (Position, Mark)
type alias Position = (Int, Int)


-- CONSTRUCT


empty : Board
empty = Board []


-- MODIFY


put : Mark -> Position -> Board -> Board
put mark p (Board moves) = Board ((p, mark) :: moves)


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
tiles (Board moves) = List.map (flip lookup moves) allPositions


-- CONSTANTS


allPositions : List Position
allPositions =
  [ (0, 0), (0, 1), (0, 2)
  , (1, 0), (1, 1), (1, 2)
  , (2, 0), (2, 1), (2, 2)
  ]


-- HELPERS


isNotMember : a -> List (a, b) -> Bool
isNotMember key = (==) Nothing << lookup key


lookup : a -> List (a, b) -> Maybe b
lookup searchKey aList =
  case aList of
    [] ->
      Nothing

    (key, value) :: rest ->
      if searchKey == key then
        Just value
      else
        lookup searchKey rest


flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b
