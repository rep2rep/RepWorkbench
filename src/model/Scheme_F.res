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

  let rec toJson = t =>
    Js.Dict.fromList(list{
      ("concept_structure", String.toJson(t.concept_structure)),
      ("concept_type", String.toJson(t.concept_type)),
      ("graphic_structure", t.graphic_structure->Option.toJson(Graphic.toJson)),
      ("graphic_type", String.toJson(t.graphic_type)),
      ("function", Function.toJson(t.function)),
      ("explicit", Bool.toJson(t.explicit)),
      ("scope", Scope.toJson(t.scope)),
      ("tokens", t.tokens->List.toJson(Token.toJson)),
      ("dimensions", t.dimensions->Non_empty_list.toJson(Dimension.toJson)),
      ("schemes", t.schemes->List.toJson(toJson)),
      ("organisation", String.toJson(t.organisation)),
    })->Js.Json.object_

  let rec fromJson = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)
      let concept_structure = get_value("concept_structure", String.fromJson)
      let concept_type = get_value("concept_type", String.fromJson)
      let graphic_structure = get_value("graphic_structure", j =>
        j->Option.fromJson(Graphic.fromJson)
      )
      let graphic_type = get_value("graphic_type", String.fromJson)
      let function = get_value("function", Function.fromJson)
      let explicit = get_value("explicit", Bool.fromJson)
      let scope = get_value("scope", Scope.fromJson)
      let tokens = get_value("tokens", j => j->List.fromJson(Token.fromJson))
      let dimensions = get_value("dimensions", j => j->Non_empty_list.fromJson(Dimension.fromJson))
      let schemes = get_value("schemes", j => j->List.fromJson(fromJson))
      let organisation = get_value("organisation", String.fromJson)
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
}
