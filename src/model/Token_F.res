module Token_level = {
  type t = Atomic | Expression

  let toJson = t =>
    switch t {
    | Atomic => Js.Json.string("Atomic")
    | Expression => Js.Json.string("Expression")
    }

  let fromJson = json =>
    switch Js.Json.decodeString(json) {
    | Some("Atomic") => Some(Atomic)
    | Some("Expression") => Some(Expression)
    | _ => None
    }
}

module Make = (Dimension: Schema_intf.S, Scheme: Schema_intf.S) => {
  type rec t = {
    concept: string,
    concept_type: string,
    graphic: option<Graphic.t>,
    graphic_type: string,
    level: Token_level.t,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Dimension.t>,
    anchored_schemes: list<Scheme.t>,
  }

  let rec validate = t =>
    t.explicit === !Option.isNone(t.graphic) &&
    t.sub_tokens->List.every(validate) &&
    t.anchored_tokens->List.every(validate) &&
    t.anchored_dimensions->List.every(Dimension.validate) &&
    t.anchored_schemes->List.every(Scheme.validate)

  let rec toJson = t =>
    Js.Dict.fromList(list{
      ("concept", Js.Json.string(t.concept)),
      ("concept_type", Js.Json.string(t.concept_type)),
      ("graphic", Option.toJson(t.graphic, Graphic.toJson)),
      ("graphic_type", Js.Json.string(t.graphic_type)),
      ("level", Token_level.toJson(t.level)),
      ("function", Function.toJson(t.function)),
      ("explicit", Js.Json.boolean(t.explicit)),
      ("sub_tokens", t.sub_tokens->List.toJson(toJson)),
      ("anchored_tokens", t.anchored_tokens->List.toJson(toJson)),
      ("anchored_dimensions", t.anchored_dimensions->List.toJson(Dimension.toJson)),
      ("anchored_schemes", t.anchored_schemes->List.toJson(Scheme.toJson)),
    })->Js.Json.object_

  let rec fromJson = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let concept = get_value("concept", Js.Json.decodeString)
      let concept_type = get_value("concept_type", Js.Json.decodeString)
      let graphic = get_value("graphic", j => j->Option.fromJson(Graphic.fromJson))
      let graphic_type = get_value("graphic_type", Js.Json.decodeString)
      let level = get_value("level", Token_level.fromJson)
      let function = get_value("function", Function.fromJson)
      let explicit = get_value("explicit", Js.Json.decodeBoolean)
      let sub_tokens = get_value("sub_tokens", j => j->List.fromJson(fromJson))
      let anchored_tokens = get_value("anchored_tokens", j => j->List.fromJson(fromJson))
      let anchored_dimensions = get_value("anchored_dimensions", j =>
        j->List.fromJson(Dimension.fromJson)
      )
      let anchored_schemes = get_value("anchored_schemes", j => j->List.fromJson(Scheme.fromJson))
      switch (
        concept,
        concept_type,
        graphic,
        graphic_type,
        level,
        function,
        explicit,
        sub_tokens,
        anchored_tokens,
        anchored_dimensions,
        anchored_schemes,
      ) {
      | (
          Some(concept),
          Some(concept_type),
          Some(graphic),
          Some(graphic_type),
          Some(level),
          Some(function),
          Some(explicit),
          Some(sub_tokens),
          Some(anchored_tokens),
          Some(anchored_dimensions),
          Some(anchored_schemes),
        ) =>
        Some({
          concept: concept,
          concept_type: concept_type,
          graphic: graphic,
          graphic_type: graphic_type,
          level: level,
          function: function,
          explicit: explicit,
          sub_tokens: sub_tokens,
          anchored_tokens: anchored_tokens,
          anchored_dimensions: anchored_dimensions,
          anchored_schemes: anchored_schemes,
        })
      | _ => None
      }
    })
}
