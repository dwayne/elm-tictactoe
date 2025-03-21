module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import XO


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { screen : Screen
    }


type Screen
    = Start
    | Playing XO.Game
    | GameOver XO.Game


init =
    { screen = Start
    }



-- UPDATE


type Msg
    = ClickedStart XO.Player
    | ClickedCell XO.Position
    | ClickedReset
    | ClickedPlayAgain
    | ClickedStop


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedStart player ->
            { model | screen = Playing (XO.start player) }

        ClickedCell pos ->
            case model.screen of
                Playing game ->
                    case XO.play pos game of
                        Ok nextGame ->
                            let
                                state =
                                    XO.toState nextGame
                            in
                            case state.outcome of
                                XO.Undecided ->
                                    { model | screen = Playing nextGame }

                                _ ->
                                    { model | screen = GameOver nextGame }

                        Err _ ->
                            model

                _ ->
                    model

        ClickedReset ->
            case model.screen of
                Playing game ->
                    { model | screen = Playing (XO.playAgain XO.defaultRules game) }

                _ ->
                    model

        ClickedPlayAgain ->
            case model.screen of
                GameOver game ->
                    { model | screen = Playing (XO.playAgain XO.defaultRules game) }

                _ ->
                    model

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

        GameOver game ->
            viewGameOverScreen game


viewStartScreen : H.Html Msg
viewStartScreen =
    H.div []
        [ H.p []
            [ H.button
                [ HE.onClick (ClickedStart XO.X) ]
                [ H.text "Start with X" ]
            ]
        , H.p []
            [ H.button
                [ HE.onClick (ClickedStart XO.O) ]
                [ H.text "Start with O" ]
            ]
        ]


viewPlayingScreen : XO.Game -> H.Html Msg
viewPlayingScreen game =
    let
        state =
            XO.toState game
    in
    H.div []
        [ H.p []
            [ H.text "Turn: "
            , H.text (playerToString state.turn)
            ]
        , viewGrid state.outcome game
        , H.p []
            [ H.button [ HE.onClick ClickedReset ] [ H.text "Reset" ]
            , H.text " "
            , H.button [ HE.onClick ClickedStop ] [ H.text "Stop" ]
            ]
        ]


viewGameOverScreen : XO.Game -> H.Html Msg
viewGameOverScreen game =
    let
        state =
            XO.toState game
    in
    H.div []
        [ H.p []
            [ H.text <|
                case state.outcome of
                    XO.Win player _ ->
                        playerToString player ++ " won"

                    XO.Draw _ ->
                        "It's a draw"

                    XO.Undecided ->
                        ""
            ]
        , viewGrid state.outcome game
        , H.p []
            [ H.button [ HE.onClick ClickedPlayAgain ] [ H.text "Play Again" ]
            , H.text " "
            , H.button [ HE.onClick ClickedStop ] [ H.text "Stop" ]
            ]
        ]


viewGrid : XO.Outcome -> XO.Game -> H.Html Msg
viewGrid outcome =
    H.div [ HA.class "grid" ] << XO.map (viewCell outcome)


viewCell : XO.Outcome -> XO.Position -> XO.Tile -> H.Html Msg
viewCell outcome pos tile =
    let
        baseAttrs =
            [ HA.classList
                [ ( "grid__cell", True )
                , ( "grid__cell--win", isWinning )
                ]
            ]

        ( additionalAttrs, text ) =
            case tile of
                Just player ->
                    ( [], playerToString player )

                Nothing ->
                    ( if isPlaying then
                        [ HE.onClick (ClickedCell pos) ]

                      else
                        []
                    , ""
                    )

        ( isWinning, isPlaying ) =
            case outcome of
                XO.Win _ lines ->
                    ( List.any (isWinningPosition pos) lines, False )

                XO.Draw _ ->
                    ( False, False )

                XO.Undecided ->
                    ( False, True )

        attrs =
            baseAttrs ++ additionalAttrs
    in
    H.div attrs [ H.text text ]



-- HELPERS


playerToString : XO.Player -> String
playerToString player =
    case player of
        XO.X ->
            "X"

        XO.O ->
            "O"


isWinningPosition : XO.Position -> XO.Line -> Bool
isWinningPosition pos ( p1, p2, p3 ) =
    pos == p1 || pos == p2 || pos == p3
