module Representation = {
  type t = {
    domain: string,
    display: string,
    notes: string,
  }

  let empty = {
    domain: "Representation",
    display: "Reference",
    notes: "",
  }
}

module Scheme = {
  type t = {
    concept_structure: string,
    graphic_structure: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    organisation: string,
    notes: string,
  }

  let empty = {
    concept_structure: "Scheme",
    graphic_structure: "Reference",
    function: Function.Semantic,
    explicit: true,
    scope: Scope.Global,
    organisation: "",
    notes: "",
  }
}

module Dimension = {
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

  let empty = {
    concept: "Dimension",
    concept_scale: Quantity_scale.Nominal,
    concept_attributes: list{},
    graphic: "Reference",
    graphic_scale: Quantity_scale.Nominal,
    graphic_attributes: list{},
    function: Function.Semantic,
    scope: Scope.Global,
    explicit: true,
    organisation: "",
    notes: "",
  }
}

module Token = {
  type t = {
    concept: string,
    graphic: string,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    notes: string,
  }

  let empty = {
    concept: "Token",
    graphic: "Reference",
    is_class: false,
    function: Function.Semantic,
    explicit: true,
    notes: "",
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
