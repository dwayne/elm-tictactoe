module AList exposing (lookup)


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
