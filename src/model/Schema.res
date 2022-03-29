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
  let _toJsonHelper: (t, Gid.Set.t) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Representation_F.Make(Token, Dimension, Scheme)

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
  let _toJsonHelper: (t, Gid.Set.t) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Dimension_F.Make(Token)

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
  let _toJsonHelper: (t, Gid.Set.t) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Scheme_F.Make(Dimension, Token)

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
  let _toJsonHelper: (t, Gid.Set.t) => (list<(Gid.t, Js.Json.t)>, Gid.Set.t)
  let _fromJsonHelper: Schema_intf.fromJsonHelper
} = Token_F.Make(Dimension, Scheme, Representation)

type t =
  | Representation(Representation.t)
  | Scheme(Scheme.t)
  | Dimension(Dimension.t)
  | Token(Token.t)

let id = t =>
  switch t {
  | Representation(r) => r.id
  | Scheme(s) => s.id
  | Dimension(d) => d.id
  | Token(t) => t.id
  }

let validate = t =>
  switch t {
  | Representation(r) => Representation.validate(r)
  | Scheme(s) => Scheme.validate(s)
  | Dimension(d) => Dimension.validate(d)
  | Token(t) => Token.validate(t)
  }

let children = t =>
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
    List.concatMany([d.dimensions->List.map(d => Dimension(d)), d.tokens->List.map(t => Token(t))])
  | Token(t) =>
    List.concatMany([
      t.sub_tokens->List.map(t => Token(t)),
      t.anchored_tokens->List.map(t => Token(t)),
      t.anchored_dimensions->List.map(d => Dimension(d)),
      t.anchored_schemes->List.map(s => Scheme(s)),
    ])
  }

let rec findById = (t, gid) =>
  if id(t) == gid {
    Some(t)
  } else {
    children(t)->List.mapPartial(t => findById(t, gid))->List.head
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
