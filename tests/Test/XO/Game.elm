module Test.XO.Game exposing (suite)


import Expect exposing (Expectation)
import Test exposing (..)
import Test.Helpers exposing (playMany)

import XO.Board as Board
import XO.Game as Game exposing (Game, Error(..))
import XO.Mark exposing (Mark(..))
import XO.Referee exposing (Outcome(..), Location(..))


suite : Test
suite =
  describe "XO.Game"
    [ playSuite
    , restartSuite
    ]


playSuite : Test
playSuite =
  describe "play"
    [ test "after 3 plays" <|
        \_ ->
          playMany X [(1, 1), (0, 2), (2, 0)]
            |> expectState O "..o.x.x.." Nothing
    , test "when X wins" <|
        \_ ->
          playMany X [(1, 1), (0, 2), (2, 0), (1, 2), (2, 2), (2, 1), (0, 0)]
            |> expectState X "x.o.xoxox" (Just <| Win D1)
    , test "when O squashes" <|
        \_ ->
          playMany O
            [ (1, 1), (0, 0), (2, 2)
            , (0, 2), (0, 1), (2, 1)
            , (1, 2), (1, 0), (2, 0)
            ]
            |> expectState O "xoxxoooxo" (Just Squash)
    , test "when the position is taken" <|
        \_ ->
          let
            game =
              playMany X [(1, 1)]
          in
          Game.play (1, 1) game
            |> Expect.equal (Err Taken)
    , test "when the position is out of bounds" <|
        \_ ->
          Game.play (0, 4) (Game.start X)
            |> Expect.equal (Err <| OutOfBounds (0, 4))
    , test "when the game is over" <|
        \_ ->
          let
            game =
              playMany O [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2)]
          in
          Game.play (1, 2) game
            |> Expect.equal (Err GameOver)
    ]


restartSuite : Test
restartSuite =
  describe "restart"
    [ test "after 3 plays on O's turn" <|
        \_ ->
          playMany X [(1, 1), (0, 2), (2, 0)]
            |> Game.restart
            |> expectState O "........." Nothing
    , test "when X wins" <|
        \_ ->
          playMany X [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2)]
            |> Game.restart
            |> expectState X "........." Nothing
    , test "when O squashes" <|
        \_ ->
          playMany O
            [ (1, 1), (0, 0), (2, 2)
            , (0, 2), (0, 1), (2, 1)
            , (1, 2), (1, 0), (2, 0)
            ]
            |> Game.restart
            |> expectState X "........." Nothing
    ]


-- HELPERS


expectState : Mark -> String -> Maybe Outcome -> Game -> Expectation
expectState turn board outcome game =
  Game.state game
    |> Expect.all
        [ (\s -> Expect.equal s.turn turn)
        , (\s -> Expect.equal (Board.toString s.board) board)
        , (\s -> Expect.equal s.outcome outcome)
        ]
