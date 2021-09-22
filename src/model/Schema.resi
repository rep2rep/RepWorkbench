module rec Representation: {
  type rec t = Schema_intf.representation = {
    uuid: Uuid.t,
    domain: string,
    display: Graphic.t,
    tokens: list<Schema_intf.token>,
    dimensions: list<Schema_intf.dimension>,
    schemes: list<Schema_intf.scheme>,
    subrepresentations: list<t>,
  }

  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
}

and Dimension: {
  type rec t = Schema_intf.dimension = {
    uuid: Uuid.t,
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
    tokens: Non_empty_list.t<Schema_intf.token>,
  }

  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
}

and Scheme: {
  type rec t = Schema_intf.scheme = {
    uuid: Uuid.t,
    concept_structure: string,
    concept_type: string,
    graphic_structure: option<Graphic.t>,
    graphic_type: string,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Schema_intf.token>,
    dimensions: Non_empty_list.t<Schema_intf.dimension>,
    schemes: list<t>,
    organisation: string,
  }

  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
}

and Token: {
  module Level: {
    type t = Token_level.t = Atomic | Expression

    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => option<t>
  }

  type rec t = Schema_intf.token = {
    uuid: Uuid.t,
    concept: string,
    concept_type: string,
    graphic: option<Graphic.t>,
    graphic_type: string,
    level: Level.t,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Schema_intf.dimension>,
    anchored_schemes: list<Schema_intf.scheme>,
  }

  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
}

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let validate: t => bool
let toJson: t => Js.Json.t
let fromJson: Js.Json.t => option<t>
