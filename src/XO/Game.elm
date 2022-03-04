module XO.Game exposing
  ( Game
  , start
  , play, restart
  , Error(..)
  , State
  , state
  , openPositions
  )


import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark)
import XO.Referee as Referee exposing (Outcome(..))


type Game
  = Playing Board Mark
  | Done Board Mark Outcome


-- CREATE


start : Mark -> Game
start = Playing Board.empty


-- MODIFY


play : Position -> Game -> Result Error Game
play p game =
  case game of
    Playing board mark ->
      if Board.inBounds p then
        if Board.isOpen p board then
          let
            nextBoard = Board.put p mark board
          in
          Ok <|
            case Referee.unsafeDecide nextBoard mark of
              Nothing ->
                Playing nextBoard <| Mark.swap mark

              Just outcome ->
                Done nextBoard mark outcome
        else
          Err Taken
      else
        Err <| OutOfBounds p

    Done _ _ _ ->
      Err GameOver


type Error
  = Taken
  | OutOfBounds Position
  | GameOver


restart : Game -> Game
restart game =
  case game of
    Playing _ mark ->
      start mark

    Done _ mark (Win _) ->
      start mark

    Done _ mark Squash ->
      start <| Mark.swap mark


-- QUERY


type alias State =
  { board : Board
  , turn : Mark
  , outcome : Maybe Outcome
  }


state : Game -> State
state game =
  case game of
    Playing board mark ->
      { board = board
      , turn = mark
      , outcome = Nothing
      }

    Done board mark outcome ->
      { board = board
      , turn = mark
      , outcome = Just outcome
      }


openPositions : Game -> List Position
openPositions game =
  case game of
    Playing board _ ->
      Board.openPositions board

    Done _ _ _ ->
      []
