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
  let _toJsonHelper: (t, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Representation_F.Make(Token_, Dimension, Scheme)

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

  let uuid: t => Uuid.t
  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
  let _toJsonHelper: (t, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Dimension_F.Make(Token_)

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

  let uuid: t => Uuid.t
  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
  let _toJsonHelper: (t, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Scheme_F.Make(Dimension, Token_)

and Token_: {
  type rec t = Schema_intf.token = {
    uuid: Uuid.t,
    concept: string,
    concept_type: string,
    graphic: option<Graphic.t>,
    graphic_type: string,
    is_class: bool,
    level: Token_level.t,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Schema_intf.dimension>,
    anchored_schemes: list<Schema_intf.scheme>,
  }

  let uuid: t => Uuid.t
  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
  let _toJsonHelper: (t, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Token_F.Make(Dimension, Scheme)

// Codegen is broken for nested modules, so we split it.
module Token: {
  module Level: {
    type t = Atomic | Expression

    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => option<t>
  }
    with type t = Token_level.t

  type rec t = Token_.t = {
    uuid: Uuid.t,
    concept: string,
    concept_type: string,
    graphic: option<Graphic.t>,
    graphic_type: string,
    is_class: bool,
    level: Level.t,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Dimension.t>,
    anchored_schemes: list<Scheme.t>,
  }

  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
} = {
  include Token_

  module Level = Token_level
}

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let validate = t =>
  switch t {
  | Representation(r) => Representation.validate(r)
  | Scheme(s) => Scheme.validate(s)
  | Dimension(d) => Dimension.validate(d)
  | Token(t) => Token.validate(t)
  }

let toJson = t => {
  let d = Js.Dict.empty()
  let set = (key, value) => Js.Dict.set(d, key, value)
  switch t {
  | Representation(r) => {
      set("type", String.toJson("Representation"))
      set("value", Representation.toJson(r))
    }
  | Scheme(s) => {
      set("type", String.toJson("Scheme"))
      set("value", Scheme.toJson(s))
    }
  | Dimension(d) => {
      set("type", String.toJson("Dimension"))
      set("value", Dimension.toJson(d))
    }
  | Token(t) => {
      set("type", String.toJson("Token"))
      set("value", Token.toJson(t))
    }
  }
  Js.Json.object_(d)
}

let fromJson = json => {
  Js.Json.decodeObject(json)->Option.flatMap(dict => {
    let read_value = decode => dict->Js.Dict.get("value")->Option.flatMap(decode)
    dict
    ->Js.Dict.get("type")
    ->Option.flatMap(s =>
      switch String.fromJson(s) {
      | Some("Representation") =>
        read_value(Representation.fromJson)->Option.map(r => Representation(r))
      | Some("Scheme") => read_value(Scheme.fromJson)->Option.map(s => Scheme(s))
      | Some("Dimension") => read_value(Dimension.fromJson)->Option.map(d => Dimension(d))
      | Some("Token") => read_value(Token.fromJson)->Option.map(t => Token(t))
      | _ => None
      }
    )
  })
}