module XO.Game exposing
  ( Game
  , start
  , play, reset
  , Error(..)
  , State
  , state
  )


import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark)
import XO.Referee as Referee exposing (Outcome(..))


type Game
  = Playing
      { first : Mark
      , turn : Mark
      , board : Board
      }
  | Done
      { first : Mark
      , turn : Mark
      , board : Board
      , outcome : Outcome
      }


-- CREATE


start : Mark -> Game
start first =
  Playing
    { first = first
    , turn = first
    , board = Board.empty
    }


-- MODIFY


play : Position -> Game -> Result Error Game
play p game =
  case game of
    Playing { first, turn, board } ->
      if Board.inBounds p then
        if Board.isOpen p board then
          let
            nextBoard = Board.put p turn board
          in
          Ok <|
            case Referee.unsafeDecide nextBoard turn of
              Nothing ->
                Playing
                  { first = first
                  , turn = Mark.swap turn
                  , board = nextBoard
                  }

              Just outcome ->
                Done
                  { first = first
                  , turn = turn
                  , board = nextBoard
                  , outcome = outcome
                  }
        else
          Err (Taken p)
      else
        Err (OutOfBounds p)

    Done _ ->
      Err GameOver


type Error
  = Taken Position
  | OutOfBounds Position
  | GameOver


reset : Game -> Game
reset game =
  case game of
    Playing { first } ->
      start first

    Done { first } ->
      start first


-- QUERY


type alias State =
  { turn : Mark
  , board : Board
  , outcome : Maybe Outcome
  }


state : Game -> State
state game =
  case game of
    Playing { turn, board } ->
      { turn = turn
      , board = board
      , outcome = Nothing
      }

    Done { turn, board, outcome } ->
      { turn = turn
      , board = board
      , outcome = Just outcome
      }
