module Make = (Dimension: Schema_intf.S, Scheme: Schema_intf.S) => {
  module Level = {
    type t = Atomic | Expression

    let to_JSON = t =>
      switch t {
      | Atomic => Js.Json.string("Atomic")
      | Expression => Js.Json.string("Expression")
      }

    let of_JSON = json =>
      switch Js.Json.decodeString(json) {
      | Some("Atomic") => Some(Atomic)
      | Some("Expression") => Some(Expression)
      | _ => None
      }
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

  let rec validate = t =>
    t.explicit === !Option.isNone(t.graphic) &&
    t.sub_tokens->List.every(validate) &&
    t.anchored_tokens->List.every(validate) &&
    t.anchored_dimensions->List.every(Dimension.validate) &&
    t.anchored_schemes->List.every(Scheme.validate)

  let rec to_JSON = t =>
    Js.Dict.fromList(list{
      ("concept", Js.Json.string(t.concept)),
      ("concept_type", Js.Json.string(t.concept_type)),
      ("graphic", Option_rep2rep.to_JSON(t.graphic, Graphic.to_JSON)),
      ("graphic_type", Js.Json.string(t.graphic_type)),
      ("level", Level.to_JSON(t.level)),
      ("function", Function.to_JSON(t.function)),
      ("explicit", Js.Json.boolean(t.explicit)),
      ("sub_tokens", t.sub_tokens->List_rep2rep.to_JSON(to_JSON)),
      ("anchored_tokens", t.anchored_tokens->List_rep2rep.to_JSON(to_JSON)),
      ("anchored_dimensions", t.anchored_dimensions->List_rep2rep.to_JSON(Dimension.to_JSON)),
      ("anchored_schemes", t.anchored_schemes->List_rep2rep.to_JSON(Scheme.to_JSON)),
    })->Js.Json.object_

  let rec of_JSON = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let concept = get_value("concept", Js.Json.decodeString)
      let concept_type = get_value("concept_type", Js.Json.decodeString)
      let graphic = get_value("graphic", j => j->Option_rep2rep.of_JSON(Graphic.of_JSON))
      let graphic_type = get_value("graphic_type", Js.Json.decodeString)
      let level = get_value("level", Level.of_JSON)
      let function = get_value("function", Function.of_JSON)
      let explicit = get_value("explicit", Js.Json.decodeBoolean)
      let sub_tokens = get_value("sub_tokens", j => j->List_rep2rep.of_JSON(of_JSON))
      let anchored_tokens = get_value("anchored_tokens", j => j->List_rep2rep.of_JSON(of_JSON))
      let anchored_dimensions = get_value("anchored_dimensions", j =>
        j->List_rep2rep.of_JSON(Dimension.of_JSON)
      )
      let anchored_schemes = get_value("anchored_schemes", j =>
        j->List_rep2rep.of_JSON(Scheme.of_JSON)
      )
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
