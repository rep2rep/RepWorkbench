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
  let to_JSON: t => Js.Json.t
  let of_JSON: Js.Json.t => option<t>
  let jsx: t => React.element
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
  let to_JSON: t => Js.Json.t
  let of_JSON: Js.Json.t => option<t>
  let jsx: t => React.element
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
  let to_JSON: t => Js.Json.t
  let of_JSON: Js.Json.t => option<t>
  let jsx: t => React.element
} = Scheme_F.Make(Dimension, Token)

and Token: {
  module Level: {
    type t = Atomic | Expression
    let to_JSON: t => Js.Json.t
    let of_JSON: Js.Json.t => option<t>
    let jsx: t => React.element
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
  let to_JSON: t => Js.Json.t
  let of_JSON: Js.Json.t => option<t>
  let jsx: t => React.element
} = Token_F.Make(Dimension, Scheme)

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

let to_JSON = t => {
  let d = Js.Dict.empty()
  let set = (key, value) => Js.Dict.set(d, key, value)
  switch t {
  | Representation(r) => {
      set("type", Js.Json.string("Representation"))
      set("value", Representation.to_JSON(r))
    }
  | Scheme(s) => {
      set("type", Js.Json.string("Scheme"))
      set("value", Scheme.to_JSON(s))
    }
  | Dimension(d) => {
      set("type", Js.Json.string("Dimension"))
      set("value", Dimension.to_JSON(d))
    }
  | Token(t) => {
      set("type", Js.Json.string("Token"))
      set("value", Token.to_JSON(t))
    }
  }
  Js.Json.object_(d)
}

let of_JSON = json => {
  Js.Json.decodeObject(json)->Option.flatMap(dict => {
    let read_value = decode => dict->Js.Dict.get("value")->Option.flatMap(decode)
    dict
    ->Js.Dict.get("type")
    ->Option.flatMap(s =>
      switch Js.Json.decodeString(s) {
      | Some("Representation") =>
        read_value(Representation.of_JSON)->Option.map(r => Representation(r))
      | Some("Scheme") => read_value(Scheme.of_JSON)->Option.map(s => Scheme(s))
      | Some("Dimension") => read_value(Dimension.of_JSON)->Option.map(d => Dimension(d))
      | Some("Token") => read_value(Token.of_JSON)->Option.map(t => Token(t))
      | _ => None
      }
    )
  })
}

let jsx = t =>
  switch t {
  | Representation(r) => Representation.jsx(r)
  | Scheme(s) => Scheme.jsx(s)
  | Dimension(d) => Dimension.jsx(d)
  | Token(t) => Token.jsx(t)
  }
