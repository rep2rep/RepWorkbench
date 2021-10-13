module Make = (
  Dimension: Schema_intf.S with type t = Schema_intf.dimension,
  Scheme: Schema_intf.S with type t = Schema_intf.scheme,
) => {
  type rec t = Schema_intf.token = {
    uuid: Uuid.t,
    concept: string,
    graphic: option<Graphic.t>,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Dimension.t>,
    anchored_schemes: list<Scheme.t>,
  }

  let uuid = t => t.uuid

  let rec validate = t =>
    Or_error.allUnit(list{
      (t.explicit === !Option.isNone(t.graphic))
        ->Or_error.fromBool_ss([
          "Token '",
          Uuid.toString(t.uuid),
          "' must be explicit iff it has a graphic instance",
        ]),
      t.sub_tokens->List.map(validate)->Or_error.allUnit,
      t.anchored_tokens->List.map(validate)->Or_error.allUnit,
      t.anchored_dimensions->List.map(Dimension.validate)->Or_error.allUnit,
      t.anchored_schemes->List.map(Scheme.validate)->Or_error.allUnit,
    })

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Uuid.Set.add(idxs, t.uuid)
    let (other_json1, idxs1) =
      t.sub_tokens->Schema_intf.collapse(List.empty, idxs0, uuid, _toJsonHelper)
    let (other_json2, idxs2) =
      t.anchored_tokens->Schema_intf.collapse(other_json1, idxs1, uuid, _toJsonHelper)
    let (other_json3, idxs3) =
      t.anchored_dimensions->Schema_intf.collapse(
        other_json2,
        idxs2,
        Dimension.uuid,
        Dimension._toJsonHelper,
      )
    let (other_json4, idxs4) =
      t.anchored_schemes->Schema_intf.collapse(
        other_json3,
        idxs3,
        Scheme.uuid,
        Scheme._toJsonHelper,
      )
    (
      other_json4->List.add((
        t.uuid,
        Js.Dict.fromList(list{
          ("concept", String.toJson(t.concept)),
          ("graphic", Option.toJson(t.graphic, Graphic.toJson)),
          ("is_class", Bool.toJson(t.is_class)),
          ("function", Function.toJson(t.function)),
          ("explicit", Bool.toJson(t.explicit)),
          ("sub_tokens", t.sub_tokens->List.map(uuid)->List.toJson(Uuid.toJson)),
          ("anchored_tokens", t.anchored_tokens->List.map(uuid)->List.toJson(Uuid.toJson)),
          (
            "anchored_dimensions",
            t.anchored_dimensions->List.map(Dimension.uuid)->List.toJson(Uuid.toJson),
          ),
          ("anchored_schemes", t.anchored_schemes->List.map(Scheme.uuid)->List.toJson(Uuid.toJson)),
        })->Js.Json.object_,
      )),
      idxs4,
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
    ->Or_error.tag("Reading Schema.Token.t")
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
        let graphic = get_value("graphic", j => j->Option.fromJson(Graphic.fromJson))
        let is_class = get_value("is_class", Bool.fromJson)
        let function = get_value("function", Function.fromJson)
        let explicit = get_value("explicit", Bool.fromJson)

        let sub_token_ids = get_value("sub_tokens", j => j->List.fromJson(Uuid.fromJson))
        let anchored_token_ids = get_value("anchored_tokens", j => j->List.fromJson(Uuid.fromJson))
        let anchored_dimension_ids = get_value("anchored_dimensions", j =>
          j->List.fromJson(Uuid.fromJson)
        )
        let anchored_scheme_ids = get_value("anchored_schemes", j =>
          j->List.fromJson(Uuid.fromJson)
        )

        sub_token_ids
        ->Or_error.tag("Reading sub-tokens")
        ->Schema_intf.recurse(
          _fromJsonHelper,
          global_dict,
          Schema_intf.Token,
          representations0,
          schemes0,
          dimensions0,
          tokens0,
        )
        ->Or_error.flatMap(((representations1, schemes1, dimensions1, tokens1)) =>
          anchored_token_ids
          ->Or_error.tag("Reading anchored tokens")
          ->Schema_intf.recurse(
            _fromJsonHelper,
            global_dict,
            Schema_intf.Token,
            representations1,
            schemes1,
            dimensions1,
            tokens1,
          )
          ->Or_error.flatMap(((representations2, schemes2, dimensions2, tokens2)) =>
            anchored_dimension_ids
            ->Or_error.tag("Reading anchored dimensions")
            ->Schema_intf.recurse(
              Dimension._fromJsonHelper,
              global_dict,
              Schema_intf.Dimension,
              representations2,
              schemes2,
              dimensions2,
              tokens2,
            )
            ->Or_error.flatMap(((representations3, schemes3, dimensions3, tokens3)) =>
              anchored_scheme_ids
              ->Or_error.tag("Reading anchored schemes")
              ->Schema_intf.recurse(
                Scheme._fromJsonHelper,
                global_dict,
                Schema_intf.Scheme,
                representations3,
                schemes3,
                dimensions3,
                tokens3,
              )
              ->Or_error.flatMap(((representations4, schemes4, dimensions4, tokens4)) => {
                let uuid_get = (map, uuid) =>
                  Uuid.Map.get(map, uuid)->Or_error.fromOption_ss([
                    "Unable to find value with UUID '",
                    Uuid.toString(uuid),
                    "' (reading Schema.Token.t)",
                  ])

                let sub_tokens =
                  sub_token_ids
                  ->Or_error.tag("Loading sub-tokens")
                  ->Or_error.flatMap(sub_token_ids =>
                    sub_token_ids->List.map(uuid => uuid_get(tokens4, uuid))->Or_error.all
                  )
                let anchored_tokens =
                  anchored_token_ids
                  ->Or_error.tag("Loading anchored tokens")
                  ->Or_error.flatMap(anchored_token_ids =>
                    anchored_token_ids->List.map(uuid => uuid_get(tokens4, uuid))->Or_error.all
                  )
                let anchored_dimensions =
                  anchored_dimension_ids
                  ->Or_error.tag("Loading anchored dimensions")
                  ->Or_error.flatMap(anchored_dimension_ids =>
                    anchored_dimension_ids
                    ->List.map(uuid => uuid_get(dimensions4, uuid))
                    ->Or_error.all
                  )
                let anchored_schemes =
                  anchored_scheme_ids
                  ->Or_error.tag("Loading anchored schemes")
                  ->Or_error.flatMap(anchored_scheme_ids =>
                    anchored_scheme_ids->List.map(uuid => uuid_get(schemes4, uuid))->Or_error.all
                  )

                Or_error.both9((
                  concept,
                  graphic,
                  is_class,
                  function,
                  explicit,
                  sub_tokens,
                  anchored_tokens,
                  anchored_dimensions,
                  anchored_schemes,
                ))->Or_error.flatMap(((
                  concept,
                  graphic,
                  is_class,
                  function,
                  explicit,
                  sub_tokens,
                  anchored_tokens,
                  anchored_dimensions,
                  anchored_schemes,
                )) => {
                  let t = {
                    uuid: uuid,
                    concept: concept,
                    graphic: graphic,
                    is_class: is_class,
                    function: function,
                    explicit: explicit,
                    sub_tokens: sub_tokens,
                    anchored_tokens: anchored_tokens,
                    anchored_dimensions: anchored_dimensions,
                    anchored_schemes: anchored_schemes,
                  }
                  Or_error.create((
                    representations4,
                    schemes4,
                    dimensions4,
                    tokens4->Uuid.Map.set(uuid, t),
                  ))->Or_error.tag(
                    Js.String2.concat("Successfully read Token with UUID ", uuid->Uuid.toString),
                  )
                })
              })
            )
          )
        )
      })
    )
  }

  let fromJson = json =>
    Js.Json.decodeObject(json)
    ->Or_error.fromOption_s("JSON is not a valid object (reading Schema.Token.t)")
    ->Or_error.flatMap(dict => {
      let uuid =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption_s("Unable to find start of model (reading Schema.Token.t)")
        ->Or_error.flatMap(Uuid.fromJson)
      uuid->Or_error.flatMap(uuid =>
        _fromJsonHelper(
          dict,
          uuid,
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
        )->Or_error.flatMap(((_, _, _, tokens)) =>
          tokens
          ->Uuid.Map.get(uuid)
          ->Or_error.fromOption_s("Missing start of model (reading Schema.Token.t)")
        )
      )
    })
}
