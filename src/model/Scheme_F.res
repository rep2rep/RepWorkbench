module Make = (
  Dimension: Schema_intf.S with type t = Schema_intf.dimension,
  Token: Schema_intf.S with type t = Schema_intf.token,
) => {
  type rec t = Schema_intf.scheme = {
    uuid: Gid.t,
    concept_structure: string,
    graphic_structure: option<Graphic.t>,
    function: Function.t,
    explicit: bool,
    scope: Scope.t,
    tokens: list<Token.t>,
    dimensions: Non_empty_list.t<Dimension.t>,
    schemes: list<t>,
    organisation: string,
  }

  let uuid = t => t.uuid

  let rec validate = t =>
    Or_error.allUnit(list{
      (t.explicit === !Option.isNone(t.graphic_structure))
        ->Or_error.fromBool_ss([
          "Scheme '",
          Gid.toString(t.uuid),
          "' must be explicit if and only if it has a graphic instance",
        ]),
      (t.dimensions->Non_empty_list.length > 2 || t.tokens->List.isEmpty->not)
        ->Or_error.fromBool_ss([
          "Scheme '",
          Gid.toString(t.uuid),
          "' must have either at least two dimensions, or at least one dimension and one token",
        ]),
      t.tokens->List.map(Token.validate)->Or_error.allUnit,
      t.dimensions->Non_empty_list.map(Dimension.validate)->Non_empty_list.toList->Or_error.allUnit,
      t.schemes->List.map(validate)->Or_error.allUnit,
    })

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Gid.Set.add(idxs, t.uuid)
    let (other_json1, idxs1) =
      t.tokens->Schema_intf.collapse(List.empty, idxs0, Token.uuid, Token._toJsonHelper)
    let (other_json2, idxs2) =
      t.dimensions
      ->Non_empty_list.toList
      ->Schema_intf.collapse(other_json1, idxs1, Dimension.uuid, Dimension._toJsonHelper)
    let (other_json3, idxs3) =
      t.schemes->Schema_intf.collapse(other_json2, idxs2, uuid, _toJsonHelper)

    (
      other_json3->List.add((
        t.uuid,
        Js.Dict.fromList(list{
          ("concept_structure", String.toJson(t.concept_structure)),
          ("graphic_structure", t.graphic_structure->Option.toJson(Graphic.toJson)),
          ("function", Function.toJson(t.function)),
          ("explicit", Bool.toJson(t.explicit)),
          ("scope", Scope.toJson(t.scope)),
          ("tokens", t.tokens->List.map(Token.uuid)->List.toJson(Gid.toJson)),
          (
            "dimensions",
            t.dimensions->Non_empty_list.map(Dimension.uuid)->Non_empty_list.toJson(Gid.toJson),
          ),
          ("schemes", t.schemes->List.map(uuid)->List.toJson(Gid.toJson)),
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
    ->List.map(((uuid, json)) => (Gid.toString(uuid), json))
    ->List.add(("start", Gid.toJson(t.uuid)))
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
    ->Js.Dict.get(uuid->Gid.toString)
    ->Or_error.fromOption_ss(["Cannot find object matching UUID '", uuid->Gid.toString, "'"])
    ->Or_error.tag("Reading Schema.Scheme.t")
    ->Or_error.tag(String.concat("Reading UUID ", uuid->Gid.toString))
    ->Or_error.flatMap(json =>
      Js.Json.decodeObject(json)
      ->Or_error.fromOption_s("JSON is not a valid object (reading Scheme.Scheme.t)")
      ->Or_error.flatMap(dict => {
        let get_value = (key, decode) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "' (reading Schema.Scheme.t)"])
          ->Or_error.flatMap(decode)

        let concept_structure = get_value("concept_structure", String.fromJson)
        let graphic_structure = get_value("graphic_structure", j =>
          j->Option.fromJson(Graphic.fromJson)
        )
        let function = get_value("function", Function.fromJson)
        let explicit = get_value("explicit", Bool.fromJson)
        let scope = get_value("scope", Scope.fromJson)
        let organisation = get_value("organisation", String.fromJson)

        let token_ids = get_value("tokens", j => j->List.fromJson(Gid.fromJson))
        let dimension_ids = get_value("dimensions", j => j->Non_empty_list.fromJson(Gid.fromJson))
        let scheme_ids = get_value("schemes", j => j->List.fromJson(Gid.fromJson))

        token_ids
        ->Or_error.tag("Reading tokens")
        ->Schema_intf.recurse(
          Token._fromJsonHelper,
          global_dict,
          Schema_intf.Token,
          representations0,
          schemes0,
          dimensions0,
          tokens0,
        )
        ->Or_error.flatMap(((representations1, schemes1, dimensions1, tokens1)) =>
          dimension_ids
          ->Or_error.tag("Reading dimensions")
          ->Or_error.map(Non_empty_list.toList)
          ->Schema_intf.recurse(
            Dimension._fromJsonHelper,
            global_dict,
            Schema_intf.Dimension,
            representations1,
            schemes1,
            dimensions1,
            tokens1,
          )
          ->Or_error.flatMap(((representations2, schemes2, dimensions2, tokens2)) =>
            scheme_ids
            ->Or_error.tag("Reading schemes")
            ->Schema_intf.recurse(
              _fromJsonHelper,
              global_dict,
              Schema_intf.Scheme,
              representations2,
              schemes2,
              dimensions2,
              tokens2,
            )
            ->Or_error.flatMap(((representations3, schemes3, dimensions3, tokens3)) => {
              let uuid_get = (map, uuid) =>
                Gid.Map.get(map, uuid)->Or_error.fromOption_ss([
                  "Unable to find value with UUID '",
                  Gid.toString(uuid),
                  "' (reading Schema.Scheme.t)",
                ])

              let tokens =
                token_ids
                ->Or_error.tag("Loading tokens")
                ->Or_error.flatMap(token_ids =>
                  token_ids->List.map(uuid => tokens3->uuid_get(uuid))->Or_error.all
                )
              let dimensions =
                dimension_ids
                ->Or_error.tag("Loading dimensions")
                ->Or_error.flatMap(dimension_ids =>
                  dimension_ids
                  ->Non_empty_list.map(uuid => dimensions3->uuid_get(uuid))
                  ->Non_empty_list.or_error_all
                )
              let schemes =
                scheme_ids
                ->Or_error.tag("Loading schemes")
                ->Or_error.flatMap(scheme_ids =>
                  scheme_ids->List.map(uuid => schemes3->uuid_get(uuid))->Or_error.all
                )

              Or_error.both9((
                concept_structure,
                graphic_structure,
                function,
                explicit,
                scope,
                tokens,
                dimensions,
                schemes,
                organisation,
              ))->Or_error.flatMap(((
                concept_structure,
                graphic_structure,
                function,
                explicit,
                scope,
                tokens,
                dimensions,
                schemes,
                organisation,
              )) => {
                let t = {
                  uuid: uuid,
                  concept_structure: concept_structure,
                  graphic_structure: graphic_structure,
                  function: function,
                  explicit: explicit,
                  scope: scope,
                  tokens: tokens,
                  dimensions: dimensions,
                  schemes: schemes,
                  organisation: organisation,
                }

                Or_error.create((
                  representations3,
                  schemes3->Gid.Map.set(uuid, t),
                  dimensions3,
                  tokens3,
                ))->Or_error.tag(
                  Js.String2.concat("Successfully read Scheme with UUID ", uuid->Gid.toString),
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
    ->Or_error.fromOption_s("JSON is not a valid object (reading Schema.Scheme.t)")
    ->Or_error.flatMap(dict => {
      let uuid =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption_s("Unable to find start of model (reading Schema.Scheme.t)")
        ->Or_error.flatMap(Gid.fromJson)
      uuid->Or_error.flatMap(uuid =>
        _fromJsonHelper(
          dict,
          uuid,
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
        )->Or_error.flatMap(((_, schemes, _, _)) =>
          schemes
          ->Gid.Map.get(uuid)
          ->Or_error.fromOption_s("Missing start of model (reading Schema.Scheme.t)")
        )
      )
    })
}
