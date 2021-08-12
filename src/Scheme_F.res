module Make = (Dimension: Schema_intf.S, Token: Schema_intf.S) => {
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

  let rec validate = t =>
    t.explicit === !Option.isNone(t.graphic_structure) &&
    t.tokens->List.every(Token.validate) &&
    t.dimensions->Non_empty_list.every(Dimension.validate) &&
    t.schemes->List.every(validate)

  let rec to_JSON = t =>
    Js.Dict.fromList(list{
      ("concept_structure", Js.Json.string(t.concept_structure)),
      ("concept_type", Js.Json.string(t.concept_type)),
      ("graphic_structure", t.graphic_structure->Option.to_JSON(Graphic.to_JSON)),
      ("graphic_type", Js.Json.string(t.graphic_type)),
      ("function", Function.to_JSON(t.function)),
      ("explicit", Js.Json.boolean(t.explicit)),
      ("scope", Scope.to_JSON(t.scope)),
      ("tokens", t.tokens->List.to_JSON(Token.to_JSON)),
      ("dimensions", t.dimensions->Non_empty_list.to_JSON(Dimension.to_JSON)),
      ("schemes", t.schemes->List.to_JSON(to_JSON)),
      ("organisation", Js.Json.string(t.organisation)),
    })->Js.Json.object_

  let rec of_JSON = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let concept_structure = get_value("concept_structure", Js.Json.decodeString)
      let concept_type = get_value("concept_type", Js.Json.decodeString)
      let graphic_structure = get_value("graphic_strucutre", j =>
        j->Option.of_JSON(Graphic.of_JSON)
      )
      let graphic_type = get_value("graphic_type", Js.Json.decodeString)
      let function = get_value("function", Function.of_JSON)
      let explicit = get_value("explicit", Js.Json.decodeBoolean)
      let scope = get_value("scope", Scope.of_JSON)
      let tokens = get_value("tokens", j => j->List.of_JSON(Token.of_JSON))
      let dimensions = get_value("dimensions", j => j->Non_empty_list.of_JSON(Dimension.of_JSON))
      let schemes = get_value("schemes", j => j->List.of_JSON(of_JSON))
      let organisation = get_value("organisation", Js.Json.decodeString)
      switch (
        concept_structure,
        concept_type,
        graphic_structure,
        graphic_type,
        function,
        explicit,
        scope,
        tokens,
        dimensions,
        schemes,
        organisation,
      ) {
      | (
          Some(concept_structure),
          Some(concept_type),
          Some(graphic_structure),
          Some(graphic_type),
          Some(function),
          Some(explicit),
          Some(scope),
          Some(tokens),
          Some(dimensions),
          Some(schemes),
          Some(organisation),
        ) =>
        Some({
          concept_structure: concept_structure,
          concept_type: concept_type,
          graphic_structure: graphic_structure,
          graphic_type: graphic_type,
          function: function,
          explicit: explicit,
          scope: scope,
          tokens: tokens,
          dimensions: dimensions,
          schemes: schemes,
          organisation: organisation,
        })
      | _ => None
      }
    })

  let jsx = t =>
    <div className="schema scheme">
      <span className="concept-structure"> {React.string(t.concept_structure)} </span>
    </div>
}
