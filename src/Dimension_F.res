module Make = (Token: Schema_intf.S) => {
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

  let rec validate = t =>
    t.concept !== "" &&
    t.explicit === !Option.isNone(t.graphic) &&
    t.dimensions->List.every(validate) &&
    t.tokens->Non_empty_list.every(Token.validate)

  let rec to_JSON = t =>
    Js.Dict.fromList(list{
      ("concept", Js.Json.string(t.concept)),
      ("concept_scale", Quantity_scale.to_JSON(t.concept_scale)),
      ("concept_type", Js.Json.string(t.concept_type)),
      ("concept_attributes", t.concept_attributes->List.to_JSON(Concept_attribute.to_JSON)),
      ("graphic", t.graphic->Option.to_JSON(Graphic.to_JSON)),
      ("graphic_scale", Quantity_scale.to_JSON(t.graphic_scale)),
      ("graphic_type", Js.Json.string(t.graphic_type)),
      ("graphic_attributes", t.graphic_attributes->List.to_JSON(Graphic_attribute.to_JSON)),
      ("function", Function.to_JSON(t.function)),
      ("scope", Scope.to_JSON(t.scope)),
      ("explicit", Js.Json.boolean(t.explicit)),
      ("dimensions", t.dimensions->List.to_JSON(to_JSON)),
      ("tokens", t.tokens->Non_empty_list.to_JSON(Token.to_JSON)),
    })->Js.Json.object_

  let rec of_JSON = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let concept = get_value("concept", Js.Json.decodeString)
      let concept_scale = get_value("concept_scale", Quantity_scale.of_JSON)
      let concept_type = get_value("concept_type", Js.Json.decodeString)
      let concept_attributes = get_value("concept_attributes", j =>
        j->List.of_JSON(Concept_attribute.of_JSON)
      )
      let graphic = get_value("graphic", j => j->Option.of_JSON(Graphic.of_JSON))
      let graphic_scale = get_value("graphic_scale", Quantity_scale.of_JSON)
      let graphic_type = get_value("graphic_type", Js.Json.decodeString)
      let graphic_attributes = get_value("graphic_attributes", j =>
        j->List.of_JSON(Graphic_attribute.of_JSON)
      )
      let function = get_value("function", Function.of_JSON)
      let scope = get_value("scope", Scope.of_JSON)
      let explicit = get_value("explicit", Js.Json.decodeBoolean)
      let dimensions = get_value("dimensions", j => j->List.of_JSON(of_JSON))
      let tokens = get_value("tokens", j => j->Non_empty_list.of_JSON(Token.of_JSON))
      switch (
        concept,
        concept_scale,
        concept_type,
        concept_attributes,
        graphic,
        graphic_scale,
        graphic_type,
        graphic_attributes,
        function,
        scope,
        explicit,
        dimensions,
        tokens,
      ) {
      | (
          Some(concept),
          Some(concept_scale),
          Some(concept_type),
          Some(concept_attributes),
          Some(graphic),
          Some(graphic_scale),
          Some(graphic_type),
          Some(graphic_attributes),
          Some(function),
          Some(scope),
          Some(explicit),
          Some(dimensions),
          Some(tokens),
        ) =>
        Some({
          concept: concept,
          concept_scale: concept_scale,
          concept_type: concept_type,
          concept_attributes: concept_attributes,
          graphic: graphic,
          graphic_scale: graphic_scale,
          graphic_type: graphic_type,
          graphic_attributes: graphic_attributes,
          function: function,
          scope: scope,
          explicit: explicit,
          dimensions: dimensions,
          tokens: tokens,
        })
      | _ => None
      }
    })

  let jsx = t =>
    <div className="schema dimension">
      <span className="concept"> {React.string(t.concept)} </span>
    </div>
}
