module Test.XO.Referee exposing (suite)


import Expect
import Test exposing (..)
import Test.Helpers exposing (putMany)

import XO.Mark exposing (Mark(..))
import XO.Referee as Referee exposing (Outcome(..), Location(..))


suite : Test
suite =
  describe "XO.Referee"
    [ unsafeDecideSuite
    ]


unsafeDecideSuite : Test
unsafeDecideSuite =
  describe "unsafeDecide"
    [ test "when X wins" <|
        \_ ->
          let
            board =
              putMany X [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2)]
          in
          Referee.unsafeDecide board X
            |> Expect.equal (Just <| Win R1)
    , test "when O wins" <|
        \_ ->
          let
            board =
              putMany O [(2, 0), (1, 0), (2, 1), (1, 1), (2, 2)]
          in
          Referee.unsafeDecide board O
            |> Expect.equal (Just <| Win R3)
    , test "when squashed" <|
        \_ ->
          let
            board =
              putMany X
                [ (0, 0), (1, 1), (2, 2)
                , (0, 1), (2, 1), (2, 0)
                , (0, 2), (1, 2), (1, 0)
                ]
          in
          Referee.unsafeDecide board X
            |> Expect.equal (Just Squash)
    , test "after 2 plays" <|
        \_ ->
          let
            board =
              putMany X [(0, 0), (1, 1)]
          in
          Referee.unsafeDecide board O
            |> Expect.equal Nothing
    ]
