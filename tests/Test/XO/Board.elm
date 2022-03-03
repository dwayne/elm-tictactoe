module Test.XO.Board exposing (suite)


import Expect
import Test exposing (..)

import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark (..))


suite : Test
suite =
  describe "XO.Board"
    [ isOpenSuite
    , openPositionsSuite
    , tilesSuite
    ]


isOpenSuite : Test
isOpenSuite =
  describe "isOpen"
    [ test "when (0, 1) does not contain a mark" <|
        \_ ->
          Board.isOpen (0, 1) Board.empty
            |> Expect.true "expected the position to be open"
    , test "when (0, 1) does contain a mark" <|
        \_ ->
          Board.isOpen (0, 1) (putMany X [(0, 1)])
            |> Expect.false "expected the position to be taken"
    ]


openPositionsSuite : Test
openPositionsSuite =
  describe "openPositions"
    [ test "when the board is empty" <|
        \_ ->
          Board.openPositions Board.empty
            |> Expect.equal
                [ (0, 0), (0, 1), (0, 2)
                , (1, 0), (1, 1), (1, 2)
                , (2, 0), (2, 1), (2, 2)
                ]
    , test "when (0, 0), (0, 2), (1, 1), (2, 0) and (2, 2) are taken" <|
        \_ ->
          Board.openPositions
            (putMany X [(0, 0), (0, 2), (1, 1), (2, 0), (2, 2)])
            |> Expect.equal [ (0, 1), (1, 0), (1, 2), (2, 1) ]
    ]


tilesSuite : Test
tilesSuite =
  describe "tiles"
    [ test "when (0, 0) and (1, 1) are taken" <|
        \_ ->
          Board.tiles
            (putMany X [(0, 0), (1, 1)])
            |> Expect.equal
                [ Just X, Nothing, Nothing
                , Nothing, Just O, Nothing
                , Nothing, Nothing, Nothing
                ]
    ]


-- HELPERS


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
