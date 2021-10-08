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

  let uuid: t => Uuid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
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
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
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
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
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
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
  let _toJsonHelper: (t, Uuid.Set.t) => (list<(Uuid.t, Js.Json.t)>, Uuid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Token_F.Make(Dimension, Scheme)

// Codegen is broken for nested modules, so we split it.
module Token: {
  module Level: {
    type t = Atomic | Expression

    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
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

  let uuid: t => Uuid.t
  let validate: t => Or_error.t<unit>
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
} = {
  include Token_

  module Level = Token_level
}

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let uuid = t =>
  switch t {
  | Representation(r) => r.uuid
  | Scheme(s) => s.uuid
  | Dimension(d) => d.uuid
  | Token(t) => t.uuid
  }

let validate = t =>
  switch t {
  | Representation(r) => Representation.validate(r)
  | Scheme(s) => Scheme.validate(s)
  | Dimension(d) => Dimension.validate(d)
  | Token(t) => Token.validate(t)
  }

let rec findByUuid = (t, uid) =>
  if uuid(t) == uid {
    Some(t)
  } else {
    switch t {
    | Representation(r) =>
      List.concatMany([
        r.tokens->List.map(t => Token(t)),
        r.schemes->List.map(s => Scheme(s)),
        r.dimensions->List.map(d => Dimension(d)),
        r.subrepresentations->List.map(r => Representation(r)),
      ])
    | Scheme(s) =>
      List.concatMany([
        s.tokens->List.map(t => Token(t)),
        s.dimensions->Non_empty_list.toList->List.map(d => Dimension(d)),
        s.schemes->List.map(s => Scheme(s)),
      ])
    | Dimension(d) =>
      List.concatMany([
        d.dimensions->List.map(d => Dimension(d)),
        d.tokens->Non_empty_list.toList->List.map(t => Token(t)),
      ])
    | Token(t) =>
      List.concatMany([
        t.sub_tokens->List.map(t => Token(t)),
        t.anchored_tokens->List.map(t => Token(t)),
        t.anchored_dimensions->List.map(d => Dimension(d)),
        t.anchored_schemes->List.map(s => Scheme(s)),
      ])
    }
    ->List.mapPartial(t => findByUuid(t, uid))
    ->List.head
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
  Js.Json.decodeObject(json)
  ->Or_error.fromOption_s("JSON is not a valid object (reading Schema.t)")
  ->Or_error.flatMap(dict => {
    let read_value = decode =>
      dict
      ->Js.Dict.get("value")
      ->Or_error.fromOption_s("Cannot find schema value to read (reading Schema.t)")
      ->Or_error.flatMap(decode)

    dict
    ->Js.Dict.get("type")
    ->Or_error.fromOption_s("Unable to determine schema type (reading Schema.t)")
    ->Or_error.flatMap(s =>
      switch String.fromJson(s)->Or_error.valOf {
      | Some("Representation") =>
        read_value(Representation.fromJson)->Or_error.map(r => Representation(r))
      | Some("Scheme") => read_value(Scheme.fromJson)->Or_error.map(s => Scheme(s))
      | Some("Dimension") => read_value(Dimension.fromJson)->Or_error.map(d => Dimension(d))
      | Some("Token") => read_value(Token.fromJson)->Or_error.map(t => Token(t))
      | _ =>
        Or_error.error_s(
          "Schema type is not one of Representation, Scheme, Dimension, or Token (reading Scheme.t)",
        )
      }
    )
  })
}
