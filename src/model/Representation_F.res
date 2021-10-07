module Make = (
  Token: Schema_intf.S with type t = Schema_intf.token,
  Dimension: Schema_intf.S with type t = Schema_intf.dimension,
  Scheme: Schema_intf.S with type t = Schema_intf.scheme,
) => {
  type rec t = Schema_intf.representation = {
    uuid: Uuid.t,
    domain: string,
    display: Graphic.t,
    tokens: list<Token.t>,
    dimensions: list<Dimension.t>,
    schemes: list<Scheme.t>,
    subrepresentations: list<t>,
  }

  let uuid = t => t.uuid

  let rec validate = t => {
    (List.length(t.tokens) > 0 || List.length(t.dimensions) > 0 || List.length(t.schemes) > 0) &&
    t.tokens->List.every(Token.validate) &&
    t.dimensions->List.every(Dimension.validate) &&
    t.schemes->List.every(Scheme.validate) &&
    t.subrepresentations->List.every(validate)
  }

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Uuid.Set.add(idxs, t.uuid)
    let (other_json1, idxs1) =
      t.tokens->Schema_intf.collapse(List.empty, idxs0, Token.uuid, Token._toJsonHelper)
    let (other_json2, idxs2) =
      t.dimensions->Schema_intf.collapse(
        other_json1,
        idxs1,
        Dimension.uuid,
        Dimension._toJsonHelper,
      )
    let (other_json3, idxs3) =
      t.schemes->Schema_intf.collapse(other_json2, idxs2, Scheme.uuid, Scheme._toJsonHelper)
    let (other_json4, idxs4) =
      t.subrepresentations->Schema_intf.collapse(other_json3, idxs3, uuid, _toJsonHelper)

    (
      other_json4->List.add((
        t.uuid,
        Js.Dict.fromList(list{
          ("domain", String.toJson(t.domain)),
          ("display", Graphic.toJson(t.display)),
          ("tokens", t.tokens->List.map(Token.uuid)->List.toJson(Uuid.toJson)),
          ("dimensions", t.dimensions->List.map(Dimension.uuid)->List.toJson(Uuid.toJson)),
          ("schemes", t.schemes->List.map(Scheme.uuid)->List.toJson(Uuid.toJson)),
          ("subrepresentations", t.subrepresentations->List.map(uuid)->List.toJson(Uuid.toJson)),
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
    ->Or_error.fromOption(
      Error.fromStrings(["Cannot find object matching UUID '", uuid->Uuid.toString, "'"]),
    )
    ->Or_error.tag("Reading Schema.Representation.t")
    ->Or_error.tag(String.concat("Reading UUID ", uuid->Uuid.toString))
    ->Or_error.flatMap(json =>
      Js.Json.decodeObject(json)
      ->Or_error.fromOption(Error.fromString("JSON is not a valid object"))
      ->Or_error.flatMap(dict => {
        let get_value = (key, decode) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption(Error.fromStrings(["Unable to find key '", key, "'"]))
          ->Or_error.flatMap(decode)

        let domain = get_value("domain", String.fromJson)
        let display = get_value("display", Graphic.fromJson)

        let token_ids = get_value("tokens", j => j->List.fromJson(Uuid.fromJson))
        let dimension_ids = get_value("dimensions", j => j->List.fromJson(Uuid.fromJson))
        let scheme_ids = get_value("schemes", j => j->List.fromJson(Uuid.fromJson))
        let subrepresentation_ids = get_value("subrepresentations", j =>
          j->List.fromJson(Uuid.fromJson)
        )

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
              Scheme._fromJsonHelper,
              global_dict,
              Schema_intf.Scheme,
              representations2,
              schemes2,
              dimensions2,
              tokens2,
            )
            ->Or_error.flatMap(((representations3, schemes3, dimensions3, tokens3)) =>
              subrepresentation_ids
              ->Or_error.tag("Reading subrepresentations")
              ->Schema_intf.recurse(
                _fromJsonHelper,
                global_dict,
                Schema_intf.Representation,
                representations3,
                schemes3,
                dimensions3,
                tokens3,
              )
              ->Or_error.flatMap(((representations4, schemes4, dimensions4, tokens4)) => {
                let uuid_get = (map, uuid) =>
                  Uuid.Map.get(map, uuid)->Or_error.fromOption(
                    Error.fromStrings([
                      "Unable to find value with UUID '",
                      Uuid.toString(uuid),
                      "'",
                    ]),
                  )

                let tokens =
                  token_ids
                  ->Or_error.tag("Loading tokens")
                  ->Or_error.flatMap(token_ids =>
                    token_ids->List.map(uuid => uuid_get(tokens4, uuid))->Or_error.all
                  )
                let dimensions =
                  dimension_ids
                  ->Or_error.tag("Loading dimensions")
                  ->Or_error.flatMap(dimension_ids =>
                    dimension_ids->List.map(uuid => uuid_get(dimensions4, uuid))->Or_error.all
                  )
                let schemes =
                  scheme_ids
                  ->Or_error.tag("Loading schemes")
                  ->Or_error.flatMap(scheme_ids =>
                    scheme_ids->List.map(uuid => uuid_get(schemes4, uuid))->Or_error.all
                  )
                let subrepresentations =
                  subrepresentation_ids
                  ->Or_error.tag("Loading subrepresentations")
                  ->Or_error.flatMap(subrepresentation_ids =>
                    subrepresentation_ids
                    ->List.map(uuid => uuid_get(representations4, uuid))
                    ->Or_error.all
                  )

                Or_error.both6((
                  domain,
                  display,
                  tokens,
                  dimensions,
                  schemes,
                  subrepresentations,
                ))->Or_error.flatMap(((
                  domain,
                  display,
                  tokens,
                  dimensions,
                  schemes,
                  subrepresentations,
                )) => {
                  let t = {
                    uuid: uuid,
                    domain: domain,
                    display: display,
                    tokens: tokens,
                    dimensions: dimensions,
                    schemes: schemes,
                    subrepresentations: subrepresentations,
                  }
                  Or_error.create((
                    representations4->Uuid.Map.set(uuid, t),
                    schemes4,
                    dimensions4,
                    tokens4,
                  ))->Or_error.tag(
                    Js.String2.concat(
                      "Successfully read Representation with UUID ",
                      uuid->Uuid.toString,
                    ),
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
    ->Or_error.fromOption(
      Error.fromString("JSON is not a valid object (reading Schema.Representation.t)"),
    )
    ->Or_error.flatMap(dict => {
      let uuid =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption(
          Error.fromString("Unable to find start of model (reading Schema.Representation.t)"),
        )
        ->Or_error.flatMap(Uuid.fromJson)
      uuid->Or_error.flatMap(uuid =>
        _fromJsonHelper(
          dict,
          uuid,
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
        )->Or_error.flatMap(((representations, _, _, _)) =>
          representations
          ->Uuid.Map.get(uuid)
          ->Or_error.fromOption(
            Error.fromString("Missing start of model (reading Schema.Representation.t)"),
          )
        )
      )
    })
}
