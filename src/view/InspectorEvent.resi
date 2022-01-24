module Representation: {
  type t =
    | Domain(string)
    | Display(string)
    | Notes(string)
}

module Scheme: {
  type t =
    | Concept_structure(string)
    | Graphic_structure(string)
    | Function(Function.t)
    | Explicit(bool)
    | Scope(Scope.t)
    | Organisation(string)
    | Notes(string)
}

module Token: {
  type t =
    | Concept(string)
    | Graphic(string)
    | Is_class(bool)
    | Function(Function.t)
    | Explicit(bool)
    | Notes(string)
}

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Token(Token.t)
