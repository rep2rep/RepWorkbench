module rec Representation: {
  type rec t = {
    domain: string,
    display: Graphic.t,
    tokens: list<Token.t>,
    dimensions: list<Dimension.t>,
    schemes: list<Scheme.t>,
    subrepresentations: list<t>,
  }

  let validate: t => bool
}

and Dimension: {
  type rec t = {
    concept: string,
    concept_scale: Quantity_scale.t,
    concept_attributes: list<Concept_attribute.t>,
    graphic: option<Graphic.t>,
    graphic_scale: Quantity_scale.t,
    graphic_attributes: list<Graphic_attribute.t>,
    function: Function.t,
    scope: Scope.t,
    explicit: bool,
    dimensions: list<t>,
    tokens: Non_empty_list.t<Token.t>,
  }

  let validate: t => bool
}

and Scheme: {
  type rec t = {
    concept_structure: string,
    graphic_structure: option<Graphic.t>,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Token.t>,
    dimensions: list<Dimension.t>,
    schemes: list<t>,
    organisation: string,
  }

  let validate: t => bool
}

and Token: {
  type rec t = {
    concept: string,
    graphic: option<Graphic.t>,
    function: Function.t,
    explicit: bool,
    tokens: list<t>,
    dimensions: list<Dimension.t>,
    schemes: list<Scheme.t>,
  }

  let validate: t => bool
}

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let validate: t => bool
