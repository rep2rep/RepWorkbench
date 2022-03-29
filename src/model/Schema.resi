module rec Representation: {
  type rec t = Schema_intf.representation = {
    id: Gid.t,
    domain: string,
    display: Graphic.t,
    tokens: list<Schema_intf.token>,
    dimensions: list<Schema_intf.dimension>,
    schemes: list<Schema_intf.scheme>,
    subrepresentations: list<t>,
  }

  let id: t => Gid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

and Dimension: {
  type rec t = Schema_intf.dimension = {
    id: Gid.t,
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
    tokens: list<Schema_intf.token>,
    organisation: string,
  }

  let id: t => Gid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

and Scheme: {
  type rec t = Schema_intf.scheme = {
    id: Gid.t,
    concept_structure: string,
    graphic_structure: option<Graphic.t>,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Schema_intf.token>,
    dimensions: Non_empty_list.t<Schema_intf.dimension>,
    schemes: list<t>,
    organisation: string,
  }

  let id: t => Gid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

and Token: {
  type rec t = Schema_intf.token = {
    id: Gid.t,
    concept: string,
    graphic: option<Graphic.t>,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Schema_intf.dimension>,
    anchored_schemes: list<Schema_intf.scheme>,
    anchored_representations: list<Schema_intf.representation>,
  }

  let id: t => Gid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let id: t => Gid.t
let findById: (t, Gid.t) => option<t>
let validate: t => Or_error.t<unit>
let children: t => list<t>

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>
