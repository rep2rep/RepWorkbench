module Representation: {
  type t = {
    domain: string,
    display: string,
    notes: string,
  }
}

module Scheme: {
  type t = {
    concept_structure: string,
    graphic_structure: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    organisation: string,
    notes: string,
  }
}

module Dimension: {
  type t = {
    concept: string,
    concept_scale: Quantity_scale.t,
    concept_attributes: list<Concept_attribute.t>,
    graphic: string,
    graphic_scale: Quantity_scale.t,
    graphic_attributes: list<Graphic_attribute.t>,
    function: Function.t,
    scope: Scope.t,
    explicit: bool,
    organisation: string,
    notes: string,
  }
}

module Token: {
  type t = {
    concept: string,
    graphic: string,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    notes: string,
  }
}

type schema =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

type t =
  | Empty
  | Multiple
  | Single(schema)
