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
} = Representation_F.Make(Token, Dimension, Scheme)

and Dimension: {
  type rec t = {
    concept: string,
    concept_scale: Quantity_scale.t,
    concept_type: string,
    concept_attributes: list<Concept_attribute.t>,
    graphic: option<Graphic.t>,
    graphic_scale: Quantity_scale.t,
    graphic_type: string,
    graphic_attributes: list<Graphic_attribute.t>,
    function: Function.t,
    scope: Scope.t,
    explicit: bool,
    dimensions: list<t>,
    tokens: Non_empty_list.t<Token.t>,
  }

  let validate: t => bool
} = Dimension_F.Make(Token)

and Scheme: {
  type rec t = {
    concept_structure: string,
    concept_type: string,
    graphic_structure: option<Graphic.t>,
    graphic_type: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Token.t>,
    dimensions: Non_empty_list.t<Dimension.t>,
    schemes: list<t>,
    organisation: string,
  }

  let validate: t => bool
} = Scheme_F.Make(Dimension, Token)

and Token: {
  module Level: {
    type t = Atomic | Expression
  }

  type rec t = {
    concept: string,
    concept_type: string,
    graphic: option<Graphic.t>,
    graphic_type: string,
    level: Level.t,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Dimension.t>,
    anchored_schemes: list<Scheme.t>,
  }

  let validate: t => bool
} = Token_F.Make(Dimension, Scheme)

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let validate = t => {
  switch t {
  | Representation(r) => Representation.validate(r)
  | Scheme(s) => Scheme.validate(s)
  | Dimension(d) => Dimension.validate(d)
  | Token(t) => Token.validate(t)
  }
}
