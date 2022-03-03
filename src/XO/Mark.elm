module XO.Mark exposing (Mark(..), swap)


type Mark
  = X
  | O


swap : Mark -> Mark
swap mark =
  case mark of
    X ->
      O

    O ->
      X
