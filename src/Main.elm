module Main exposing (main)


import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import XO.Board as Board exposing (Board, Cell, Position)
import XO.Game as Game exposing (Game)
import XO.Mark as Mark exposing (Mark(..))
import XO.Referee as Referee exposing (Outcome(..))


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { screen : Screen
  }


type Screen
  = Start
  | Playing Game
  | GameOver Board Outcome


init =
  { screen = Start
  }


-- UPDATE


type Msg
  = ClickedStart Mark
  | ClickedCell Position
  | ClickedReset
  | ClickedPlayAgain
  | ClickedStop


update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickedStart mark ->
      { model | screen = Playing (Game.start mark) }

    ClickedCell p ->
      case model.screen of
        Playing game ->
          case Game.play p game of
            Ok nextGame ->
              let
                state =
                  Game.state nextGame
              in
              case state.outcome of
                Nothing ->
                  { model | screen = Playing nextGame }

                Just outcome ->
                  { model | screen = GameOver state.board outcome }

            Err _ -> -- UNREACHABLE
              model

        _ ->
          model

    ClickedReset ->
      case model.screen of
        Playing game ->
          { model | screen = Playing (Game.reset game) }

        _ ->
          model

    ClickedPlayAgain ->
      case model.screen of
        GameOver _ outcome ->
          case outcome of
            Win mark _ ->
              { model | screen = Playing (Game.start mark) }

            Draw mark ->
              { model | screen = Playing (Game.start (Mark.swap mark)) }

        _ ->
          model
    --
    ClickedStop ->
      { model | screen = Start }


-- VIEW


view : Model -> H.Html Msg
view { screen } =
  case screen of
    Start ->
      viewStartScreen

    Playing game ->
      viewPlayingScreen game

    GameOver board outcome ->
      viewGameOverScreen board outcome


viewStartScreen : H.Html Msg
viewStartScreen =
  H.div []
    [ H.p []
        [ H.button
            [ HE.onClick (ClickedStart X) ]
            [ H.text "Start with X" ]
        ]
    , H.p []
        [ H.button
            [ HE.onClick (ClickedStart O) ]
            [ H.text "Start with O" ]
        ]
    ]


viewPlayingScreen : Game -> H.Html Msg
viewPlayingScreen game =
  let
    state =
      Game.state game
  in
  H.div []
    [ H.p []
        [ H.text "Turn: "
        , H.text (Mark.toString state.turn)
        ]
    , viewGrid Nothing (Board.cells state.board)
    , H.p []
        [ H.button [ HE.onClick ClickedReset ] [ H.text "Reset" ]
        , H.text " "
        , H.button [ HE.onClick ClickedStop ] [ H.text "Stop" ]
        ]
    ]


viewGameOverScreen : Board -> Outcome -> H.Html Msg
viewGameOverScreen board outcome =
  H.div []
    [ H.p []
        [ H.text <|
            case outcome of
              Win mark _ ->
                Mark.toString mark ++ " won"

              Draw _ ->
                "It's a draw"
        ]
    , viewGrid (Just outcome) (Board.cells board)
    , H.p []
        [ H.button [ HE.onClick ClickedPlayAgain ] [ H.text "Play Again" ]
        , H.text " "
        , H.button [ HE.onClick ClickedStop ] [ H.text "Stop" ]
        ]
    ]


viewGrid : Maybe Outcome -> List Cell -> H.Html Msg
viewGrid outcome =
  H.div [ HA.class "grid" ] << List.map (viewCell outcome)


viewCell : Maybe Outcome -> Cell -> H.Html Msg
viewCell maybeOutcome (p, tile) =
  let
    baseAttrs =
      [ HA.classList
          [ ("grid__cell", True)
          , ("grid__cell--win", isWinningCell)
          ]
      ]

    (additionalAttrs, tileText) =
      case tile of
        Nothing ->
          ( if isPlaying then
              [ HE.onClick (ClickedCell p) ]
            else
              []
          , ""
          )

        Just mark ->
          ( [], Mark.toString mark )

    (isWinningCell, isPlaying) =
      case maybeOutcome of
        Just outcome ->
          (Referee.isWinningPosition p outcome, False)

        Nothing ->
          (False, True)

    attrs =
      baseAttrs ++ additionalAttrs
  in
  H.div attrs [ H.text tileText ]
