module Make = (
  Dimension: Schema_intf.S with type t = Schema_intf.dimension,
  Scheme: Schema_intf.S with type t = Schema_intf.scheme,
  Representation: Schema_intf.S with type t = Schema_intf.representation,
) => {
  type rec t = Schema_intf.token = {
    id: Gid.t,
    concept: string,
    graphic: option<Graphic.t>,
    is_class: bool,
    function: Function.t,
    explicit: bool,
    sub_tokens: list<t>,
    anchored_tokens: list<t>,
    anchored_dimensions: list<Dimension.t>,
    anchored_schemes: list<Scheme.t>,
    anchored_representations: list<Representation.t>,
  }

  let id = t => t.id

  let rec validate = t =>
    Or_error.allUnit(list{
      (t.explicit === !Option.isNone(t.graphic))
        ->Or_error.fromBool_ss([
          "Token '",
          Gid.toString(t.id),
          "' must be explicit iff it has a graphic instance",
        ]),
      t.sub_tokens->List.map(validate)->Or_error.allUnit,
      t.anchored_tokens->List.map(validate)->Or_error.allUnit,
      t.anchored_dimensions->List.map(Dimension.validate)->Or_error.allUnit,
      t.anchored_schemes->List.map(Scheme.validate)->Or_error.allUnit,
      t.anchored_representations->List.map(Representation.validate)->Or_error.allUnit,
    })

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Gid.Set.add(idxs, t.id)
    let (other_json1, idxs1) =
      t.sub_tokens->Schema_intf.collapse(List.empty, idxs0, id, _toJsonHelper)
    let (other_json2, idxs2) =
      t.anchored_tokens->Schema_intf.collapse(other_json1, idxs1, id, _toJsonHelper)
    let (other_json3, idxs3) =
      t.anchored_dimensions->Schema_intf.collapse(
        other_json2,
        idxs2,
        Dimension.id,
        Dimension._toJsonHelper,
      )
    let (other_json4, idxs4) =
      t.anchored_schemes->Schema_intf.collapse(other_json3, idxs3, Scheme.id, Scheme._toJsonHelper)
    let (other_json5, idxs5) =
      t.anchored_representations->Schema_intf.collapse(
        other_json4,
        idxs4,
        Representation.id,
        Representation._toJsonHelper,
      )

    (
      other_json5->List.add((
        t.id,
        Js.Dict.fromList(list{
          ("concept", String.toJson(t.concept)),
          ("graphic", Option.toJson(t.graphic, Graphic.toJson)),
          ("is_class", Bool.toJson(t.is_class)),
          ("function", Function.toJson(t.function)),
          ("explicit", Bool.toJson(t.explicit)),
          ("sub_tokens", t.sub_tokens->List.map(id)->List.toJson(Gid.toJson)),
          ("anchored_tokens", t.anchored_tokens->List.map(id)->List.toJson(Gid.toJson)),
          (
            "anchored_dimensions",
            t.anchored_dimensions->List.map(Dimension.id)->List.toJson(Gid.toJson),
          ),
          ("anchored_schemes", t.anchored_schemes->List.map(Scheme.id)->List.toJson(Gid.toJson)),
          (
            "anchored_representations",
            t.anchored_representations->List.map(Representation.id)->List.toJson(Gid.toJson),
          ),
        })->Js.Json.object_,
      )),
      idxs5,
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
    ->Or_error.tag("Reading Schema.Token.t")
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
        let graphic = get_value("graphic", j => j->Option.fromJson(Graphic.fromJson))
        let is_class = get_value("is_class", Bool.fromJson)
        let function = get_value("function", Function.fromJson)
        let explicit = get_value("explicit", Bool.fromJson)

        let sub_token_ids = get_value("sub_tokens", j => j->List.fromJson(Gid.fromJson))
        let anchored_token_ids = get_value("anchored_tokens", j => j->List.fromJson(Gid.fromJson))
        let anchored_dimension_ids = get_value("anchored_dimensions", j =>
          j->List.fromJson(Gid.fromJson)
        )
        let anchored_scheme_ids = get_value("anchored_schemes", j => j->List.fromJson(Gid.fromJson))
        let anchored_representation_ids = get_value("anchored_representations", j =>
          j->List.fromJson(Gid.fromJson)
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
              ->Or_error.flatMap(((representations4, schemes4, dimensions4, tokens4)) =>
                anchored_representation_ids
                ->Or_error.tag("Reading anchored schemes")
                ->Schema_intf.recurse(
                  Representation._fromJsonHelper,
                  global_dict,
                  Schema_intf.Representation,
                  representations4,
                  schemes4,
                  dimensions4,
                  tokens4,
                )
                ->Or_error.flatMap(((representations5, schemes5, dimensions5, tokens5)) => {
                  let id_get = (map, id) =>
                    Gid.Map.get(map, id)->Or_error.fromOption_ss([
                      "Unable to find value with ID '",
                      Gid.toString(id),
                      "' (reading Schema.Token.t)",
                    ])

                  let sub_tokens =
                    sub_token_ids
                    ->Or_error.tag("Loading sub-tokens")
                    ->Or_error.flatMap(sub_token_ids =>
                      sub_token_ids->List.map(id => id_get(tokens5, id))->Or_error.all
                    )
                  let anchored_tokens =
                    anchored_token_ids
                    ->Or_error.tag("Loading anchored tokens")
                    ->Or_error.flatMap(anchored_token_ids =>
                      anchored_token_ids->List.map(id => id_get(tokens5, id))->Or_error.all
                    )
                  let anchored_dimensions =
                    anchored_dimension_ids
                    ->Or_error.tag("Loading anchored dimensions")
                    ->Or_error.flatMap(anchored_dimension_ids =>
                      anchored_dimension_ids->List.map(id => id_get(dimensions5, id))->Or_error.all
                    )
                  let anchored_schemes =
                    anchored_scheme_ids
                    ->Or_error.tag("Loading anchored schemes")
                    ->Or_error.flatMap(anchored_scheme_ids =>
                      anchored_scheme_ids->List.map(id => id_get(schemes5, id))->Or_error.all
                    )
                  let anchored_representations =
                    anchored_representation_ids
                    ->Or_error.tag("Loading anchored representations")
                    ->Or_error.flatMap(anchored_representation_ids =>
                      anchored_representation_ids
                      ->List.map(id => id_get(representations5, id))
                      ->Or_error.all
                    )

                  Or_error.both10((
                    concept,
                    graphic,
                    is_class,
                    function,
                    explicit,
                    sub_tokens,
                    anchored_tokens,
                    anchored_dimensions,
                    anchored_schemes,
                    anchored_representations,
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
                    anchored_representations,
                  )) => {
                    let t = {
                      id: id,
                      concept: concept,
                      graphic: graphic,
                      is_class: is_class,
                      function: function,
                      explicit: explicit,
                      sub_tokens: sub_tokens,
                      anchored_tokens: anchored_tokens,
                      anchored_dimensions: anchored_dimensions,
                      anchored_schemes: anchored_schemes,
                      anchored_representations: anchored_representations,
                    }
                    Or_error.create((
                      representations4,
                      schemes4,
                      dimensions4,
                      tokens4->Gid.Map.set(id, t),
                    ))->Or_error.tag(
                      Js.String2.concat("Successfully read Token with ID ", id->Gid.toString),
                    )
                  })
                })
              )
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
      let id =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption_s("Unable to find start of model (reading Schema.Token.t)")
        ->Or_error.flatMap(Gid.fromJson)
      id->Or_error.flatMap(id =>
        _fromJsonHelper(
          dict,
          id,
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
        )->Or_error.flatMap(((_, _, _, tokens)) =>
          tokens
          ->Gid.Map.get(id)
          ->Or_error.fromOption_s("Missing start of model (reading Schema.Token.t)")
        )
      )
    })
}
