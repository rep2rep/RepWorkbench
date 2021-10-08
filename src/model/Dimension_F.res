module Make = (Token: Schema_intf.S with type t = Schema_intf.token) => {
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
    tokens: Non_empty_list.t<Token.t>,
  }

  let uuid = t => t.uuid

  let rec validate = t =>
    Or_error.allUnit(list{
      (t.concept !== "")->Or_error.fromBool_s("Dimension concept cannot be empty"),
      (t.explicit === !Option.isNone(t.graphic))
        ->Or_error.fromBool_ss([
          "Dimension '",
          Uuid.toString(t.uuid),
          "' must be explicit if and only if it has a graphic instance",
        ]),
      t.dimensions->List.map(validate)->Or_error.allUnit,
      t.tokens->Non_empty_list.map(Token.validate)->Non_empty_list.toList->Or_error.allUnit,
    })

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Uuid.Set.add(idxs, t.uuid)
    let (other_json1, idxs1) =
      t.dimensions->Schema_intf.collapse(List.empty, idxs0, uuid, _toJsonHelper)
    let (other_json2, idxs2) =
      t.tokens
      ->Non_empty_list.toList
      ->Schema_intf.collapse(other_json1, idxs1, Token.uuid, Token._toJsonHelper)

    (
      other_json2->List.add((
        t.uuid,
        Js.Dict.fromList(list{
          ("concept", String.toJson(t.concept)),
          ("concept_scale", Quantity_scale.toJson(t.concept_scale)),
          ("concept_type", String.toJson(t.concept_type)),
          ("concept_attributes", t.concept_attributes->List.toJson(Concept_attribute.toJson)),
          ("graphic", t.graphic->Option.toJson(Graphic.toJson)),
          ("graphic_scale", Quantity_scale.toJson(t.graphic_scale)),
          ("graphic_type", String.toJson(t.graphic_type)),
          ("graphic_attributes", t.graphic_attributes->List.toJson(Graphic_attribute.toJson)),
          ("function", Function.toJson(t.function)),
          ("scope", Scope.toJson(t.scope)),
          ("explicit", Bool.toJson(t.explicit)),
          ("dimensions", t.dimensions->List.map(uuid)->List.toJson(Uuid.toJson)),
          ("tokens", t.tokens->Non_empty_list.map(Token.uuid)->Non_empty_list.toJson(Uuid.toJson)),
        })->Js.Json.object_,
      )),
      idxs2,
    )
  }

  let toJson = t =>
    t
    ->_toJsonHelper(Uuid.Set.empty)
    ->(((json, _)) => json)
    ->List.map(((uuid, json)) => (Uuid.toString(uuid), json))
    ->List.add(("start", Uuid.toJson(t.uuid)))
    ->Js.Dict.fromList
    ->Js.Json.object_

  let rec _fromJsonHelper = (
    global_dict,
    uuid,
    representations0,
    schemes0,
    dimensions0,
    tokens0,
  ) => {
    global_dict
    ->Js.Dict.get(uuid->Uuid.toString)
    ->Or_error.fromOption_ss(["Cannot find object matching UUID '", uuid->Uuid.toString, "'"])
    ->Or_error.tag("Reading Schema.Dimension.t")
    ->Or_error.tag(String.concat("Reading UUID ", uuid->Uuid.toString))
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
        let concept_type = get_value("concept_type", String.fromJson)
        let concept_attributes = get_value("concept_attributes", j =>
          j->List.fromJson(Concept_attribute.fromJson)
        )
        let graphic = get_value("graphic", j => j->Option.fromJson(Graphic.fromJson))
        let graphic_scale = get_value("graphic_scale", Quantity_scale.fromJson)
        let graphic_type = get_value("graphic_type", String.fromJson)
        let graphic_attributes = get_value("graphic_attributes", j =>
          j->List.fromJson(Graphic_attribute.fromJson)
        )
        let function = get_value("function", Function.fromJson)
        let scope = get_value("scope", Scope.fromJson)
        let explicit = get_value("explicit", Bool.fromJson)

        let dimension_ids = get_value("dimensions", j => j->List.fromJson(Uuid.fromJson))
        let token_ids = get_value("tokens", j => j->Non_empty_list.fromJson(Uuid.fromJson))

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
          ->Or_error.map(Non_empty_list.toList)
          ->Schema_intf.recurse(
            Token._fromJsonHelper,
            global_dict,
            Schema_intf.Token,
            representations1,
            schemes1,
            dimensions1,
            tokens1,
          )
          ->Or_error.flatMap(((representations2, schemes2, dimensions2, tokens2)) => {
            let uuid_get = (map, uuid) =>
              Uuid.Map.get(map, uuid)->Or_error.fromOption_ss([
                "Unable to find value with UUID '",
                Uuid.toString(uuid),
                "' (reading Schema.Scheme.t)",
              ])

            let dimensions =
              dimension_ids
              ->Or_error.tag("Loading dimensions")
              ->Or_error.flatMap(dimension_ids =>
                dimension_ids->List.map(uuid => dimensions2->uuid_get(uuid))->Or_error.all
              )
            let tokens =
              token_ids
              ->Or_error.tag("Loading tokens")
              ->Or_error.flatMap(token_ids =>
                token_ids
                ->Non_empty_list.map(uuid => tokens2->uuid_get(uuid))
                ->Non_empty_list.or_error_all
              )

            Or_error.both13((
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
            ))->Or_error.flatMap(((
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
            )) => {
              let t = {
                uuid: uuid,
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
              }

              Or_error.create((
                representations2,
                schemes2,
                dimensions2->Uuid.Map.set(uuid, t),
                tokens2,
              ))->Or_error.tag(
                Js.String2.concat("Successfully read Dimension with UUID ", uuid->Uuid.toString),
              )
            })
          })
        )
      })
    )
  }

  let fromJson = json =>
    Js.Json.decodeObject(json)
    ->Or_error.fromOption_s("JSON is not a valid object (reading Schema.Dimension.t)")
    ->Or_error.flatMap(dict => {
      let uuid =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption_s("Unable to find start of model (reading Schema.Dimension.t)")
        ->Or_error.flatMap(Uuid.fromJson)
      uuid->Or_error.flatMap(uuid =>
        _fromJsonHelper(
          dict,
          uuid,
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
        )->Or_error.flatMap(((_, _, dimensions, _)) =>
          dimensions
          ->Uuid.Map.get(uuid)
          ->Or_error.fromOption_s("Missing start of model (reading Schema.Dimension.t)")
        )
      )
    })
}
