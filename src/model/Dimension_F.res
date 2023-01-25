module Make = (Token: Schema_intf.S with type t = Schema_intf.token) => {
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
    tokens: list<Token.t>,
    anchored_dimensions: list<t>,
    organisation: string,
  }

  let id = t => t.id

  let rec validate = t =>
    Or_error.allUnit(list{
      (t.concept !== "")->Or_error.fromBool_s("Dimension concept cannot be empty"),
      (t.explicit === !Option.isNone(t.graphic))
        ->Or_error.fromBool_ss([
          "Dimension '",
          Gid.toString(t.id),
          "' must be explicit if and only if it has a graphic instance",
        ]),
      t.dimensions->List.map(validate)->Or_error.allUnit,
      t.tokens->List.map(Token.validate)->Or_error.allUnit,
      t.anchored_dimensions->List.map(validate)->Or_error.allUnit,
      (List.length(t.dimensions) + List.length(t.tokens) + List.length(t.anchored_dimensions) > 0)
        ->Or_error.fromBool_ss([
          "Dimension '",
          Gid.toString(t.id),
          "' must have at least one child schema.",
        ]),
    })

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Gid.Set.add(idxs, t.id)
    let (other_json1, idxs1) =
      t.dimensions->Schema_intf.collapse(List.empty, idxs0, id, _toJsonHelper)
    let (other_json2, idxs2) =
      t.tokens->Schema_intf.collapse(other_json1, idxs1, Token.id, Token._toJsonHelper)
    let (other_json3, idxs3) =
      t.anchored_dimensions->Schema_intf.collapse(other_json2, idxs2, id, _toJsonHelper)

    (
      other_json3->List.add((
        t.id,
        Js.Dict.fromList(list{
          ("concept", String.toJson(t.concept)),
          ("concept_scale", Quantity_scale.toJson(t.concept_scale)),
          ("concept_attributes", t.concept_attributes->List.toJson(Concept_attribute.toJson)),
          ("graphic", t.graphic->Option.toJson(Graphic.toJson)),
          ("graphic_scale", Quantity_scale.toJson(t.graphic_scale)),
          ("graphic_attributes", t.graphic_attributes->List.toJson(Graphic_attribute.toJson)),
          ("function", Function.toJson(t.function)),
          ("scope", Scope.toJson(t.scope)),
          ("explicit", Bool.toJson(t.explicit)),
          ("dimensions", t.dimensions->List.map(id)->List.toJson(Gid.toJson)),
          ("tokens", t.tokens->List.map(Token.id)->List.toJson(Gid.toJson)),
          ("anchored_dimensions", t.anchored_dimensions->List.map(id)->List.toJson(Gid.toJson)),
          ("organisation", String.toJson(t.organisation)),
        })->Js.Json.object_,
      )),
      idxs3,
    )
  }

  let toJson = t =>
    t
    ->_toJsonHelper(Gid.Set.empty)
    ->(((json, _)) => json)
    ->List.map(((id, json)) => (Gid.toString(id), json))
    ->List.add(("start", Gid.toJson(t.id)))
    ->Js.Dict.fromList
    ->Js.Json.object_

  let rec _fromJsonHelper = (global_dict, id, representations0, schemes0, dimensions0, tokens0) => {
    global_dict
    ->Js.Dict.get(id->Gid.toString)
    ->Or_error.fromOption_ss(["Cannot find object matching ID '", id->Gid.toString, "'"])
    ->Or_error.tag("Reading Schema.Dimension.t")
    ->Or_error.tag(String.concat("Reading ID ", id->Gid.toString))
    ->Or_error.flatMap(json =>
      Js.Json.decodeObject(json)
      ->Or_error.fromOption_s("JSON is not a valid object")
      ->Or_error.flatMap(dict => {
        let get_value = (key, decode) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(decode)

        let concept = get_value("concept", String.fromJson)
        let concept_scale = get_value("concept_scale", Quantity_scale.fromJson)
        let concept_attributes = get_value("concept_attributes", j =>
          j->List.fromJson(Concept_attribute.fromJson)
        )
        let graphic = get_value("graphic", j => j->Option.fromJson(Graphic.fromJson))
        let graphic_scale = get_value("graphic_scale", Quantity_scale.fromJson)
        let graphic_attributes = get_value("graphic_attributes", j =>
          j->List.fromJson(Graphic_attribute.fromJson)
        )
        let function = get_value("function", Function.fromJson)
        let scope = get_value("scope", Scope.fromJson)
        let explicit = get_value("explicit", Bool.fromJson)
        let organisation = get_value("organisation", String.fromJson)

        let dimension_ids = get_value("dimensions", j => j->List.fromJson(Gid.fromJson))
        let token_ids = get_value("tokens", j => j->List.fromJson(Gid.fromJson))
        let anchored_dimension_ids = get_value(
          "anchored_dimensions",
          List.fromJson(_, Gid.fromJson),
        )

        dimension_ids
        ->Or_error.tag("Reading dimensions")
        ->Schema_intf.recurse(
          _fromJsonHelper,
          global_dict,
          Schema_intf.Dimension,
          representations0,
          schemes0,
          dimensions0,
          tokens0,
        )
        ->Or_error.flatMap(((representations1, schemes1, dimensions1, tokens1)) =>
          token_ids
          ->Or_error.tag("Reading tokens")
          ->Schema_intf.recurse(
            Token._fromJsonHelper,
            global_dict,
            Schema_intf.Token,
            representations1,
            schemes1,
            dimensions1,
            tokens1,
          )
          ->Or_error.flatMap(((representations2, schemes2, dimensions2, tokens2)) =>
            anchored_dimension_ids
            ->Or_error.tag("Reading tokens")
            ->Schema_intf.recurse(
              _fromJsonHelper,
              global_dict,
              Schema_intf.Dimension,
              representations2,
              schemes2,
              dimensions2,
              tokens2,
            )
            ->Or_error.flatMap(((representations3, schemes3, dimensions3, tokens3)) => {
              let id_get = (map, id) =>
                Gid.Map.get(map, id)->Or_error.fromOption_ss([
                  "Unable to find value with ID '",
                  Gid.toString(id),
                  "' (reading Schema.Scheme.t)",
                ])

              let dimensions =
                dimension_ids
                ->Or_error.tag("Loading dimensions")
                ->Or_error.flatMap(dimension_ids =>
                  dimension_ids->List.map(id => dimensions3->id_get(id))->Or_error.all
                )
              let tokens =
                token_ids
                ->Or_error.tag("Loading tokens")
                ->Or_error.flatMap(token_ids =>
                  token_ids->List.map(id => tokens3->id_get(id))->Or_error.all
                )
              let anchored_dimensions =
                anchored_dimension_ids
                ->Or_error.tag("Loading anchored dimensions")
                ->Or_error.flatMap(anchored_dimension_ids =>
                  anchored_dimension_ids->List.map(id => dimensions3->id_get(id))->Or_error.all
                )

              Or_error.both13((
                concept,
                concept_scale,
                concept_attributes,
                graphic,
                graphic_scale,
                graphic_attributes,
                function,
                scope,
                explicit,
                dimensions,
                tokens,
                anchored_dimensions,
                organisation,
              ))->Or_error.flatMap(((
                concept,
                concept_scale,
                concept_attributes,
                graphic,
                graphic_scale,
                graphic_attributes,
                function,
                scope,
                explicit,
                dimensions,
                tokens,
                anchored_dimensions,
                organisation,
              )) => {
                let t = {
                  id: id,
                  concept: concept,
                  concept_scale: concept_scale,
                  concept_attributes: concept_attributes,
                  graphic: graphic,
                  graphic_scale: graphic_scale,
                  graphic_attributes: graphic_attributes,
                  function: function,
                  scope: scope,
                  explicit: explicit,
                  dimensions: dimensions,
                  tokens: tokens,
                  anchored_dimensions: anchored_dimensions,
                  organisation: organisation,
                }

                Or_error.create((
                  representations3,
                  schemes3,
                  dimensions3->Gid.Map.set(id, t),
                  tokens3,
                ))->Or_error.tag(
                  Js.String2.concat("Successfully read Dimension with ID ", id->Gid.toString),
                )
              })
            })
          )
        )
      })
    )
  }

  let fromJson = json =>
    Js.Json.decodeObject(json)
    ->Or_error.fromOption_s("JSON is not a valid object (reading Schema.Dimension.t)")
    ->Or_error.flatMap(dict => {
      let id =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption_s("Unable to find start of model (reading Schema.Dimension.t)")
        ->Or_error.flatMap(Gid.fromJson)
      id->Or_error.flatMap(id =>
        _fromJsonHelper(
          dict,
          id,
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
        )->Or_error.flatMap(((_, _, dimensions, _)) =>
          dimensions
          ->Gid.Map.get(id)
          ->Or_error.fromOption_s("Missing start of model (reading Schema.Dimension.t)")
        )
      )
    })
}
