module Representation: {
  type t =
    | Domain(string)
    | Display(string)
    | Notes(string)
}

type t = Representation(Representation.t)
