module Representation = {
  type t =
    | Domain(string)
    | Display(string)
    | Notes(string)
}

module Scheme = {
  type t =
    | Concept_structure(string)
    | Graphic_structure(string)
    | Function(Function.t)
    | Explicit(bool)
    | Scope(Scope.t)
    | Organisation(string)
    | Notes(string)
}

module Dimension = {
  type t =
    | Concept(string)
    | Concept_scale(Quantity_scale.t)
    | Concept_attributes(list<Concept_attribute.t>)
    | Graphic(string)
    | Graphic_scale(Quantity_scale.t)
    | Graphic_attributes(list<Graphic_attribute.t>)
    | Function(Function.t)
    | Scope(Scope.t)
    | Explicit(bool)
    | Organisation(string)
    | Notes(string)
}

module Token = {
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
  | Dimension(Dimension.t)
  | Token(Token.t)
