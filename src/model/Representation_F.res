module Make = (
  Token: Schema_intf.S with type t = Schema_intf.token,
  Dimension: Schema_intf.S with type t = Schema_intf.dimension,
  Scheme: Schema_intf.S with type t = Schema_intf.scheme,
) => {
  type rec t = Schema_intf.representation = {
    id: Gid.t,
    domain: string,
    display: Graphic.t,
    tokens: list<Token.t>,
    dimensions: list<Dimension.t>,
    schemes: list<Scheme.t>,
    subrepresentations: list<t>,
  }

  let id = t => t.id

  let rec validate = t => {
    Or_error.allUnit(list{
      (List.length(t.tokens) > 0 || List.length(t.dimensions) > 0 || List.length(t.schemes) > 0)
        ->Or_error.fromBool_ss([
          "Representation '",
          Gid.toString(t.id),
          "' must have at least one token, dimension, or scheme",
        ]),
      t.tokens->List.map(Token.validate)->Or_error.allUnit,
      t.dimensions->List.map(Dimension.validate)->Or_error.allUnit,
      t.schemes->List.map(Scheme.validate)->Or_error.allUnit,
      t.subrepresentations->List.map(validate)->Or_error.allUnit,
    })
  }

  let rec _toJsonHelper = (t, idxs) => {
    let idxs0 = Gid.Set.add(idxs, t.id)
    let (other_json1, idxs1) =
      t.tokens->Schema_intf.collapse(List.empty, idxs0, Token.id, Token._toJsonHelper)
    let (other_json2, idxs2) =
      t.dimensions->Schema_intf.collapse(other_json1, idxs1, Dimension.id, Dimension._toJsonHelper)
    let (other_json3, idxs3) =
      t.schemes->Schema_intf.collapse(other_json2, idxs2, Scheme.id, Scheme._toJsonHelper)
    let (other_json4, idxs4) =
      t.subrepresentations->Schema_intf.collapse(other_json3, idxs3, id, _toJsonHelper)

    (
      other_json4->List.add((
        t.id,
        Js.Dict.fromList(list{
          ("domain", String.toJson(t.domain)),
          ("display", Graphic.toJson(t.display)),
          ("tokens", t.tokens->List.map(Token.id)->List.toJson(Gid.toJson)),
          ("dimensions", t.dimensions->List.map(Dimension.id)->List.toJson(Gid.toJson)),
          ("schemes", t.schemes->List.map(Scheme.id)->List.toJson(Gid.toJson)),
          ("subrepresentations", t.subrepresentations->List.map(id)->List.toJson(Gid.toJson)),
        })->Js.Json.object_,
      )),
      idxs4,
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
    ->Or_error.tag("Reading Schema.Representation.t")
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

        let domain = get_value("domain", String.fromJson)
        let display = get_value("display", Graphic.fromJson)

        let token_ids = get_value("tokens", j => j->List.fromJson(Gid.fromJson))
        let dimension_ids = get_value("dimensions", j => j->List.fromJson(Gid.fromJson))
        let scheme_ids = get_value("schemes", j => j->List.fromJson(Gid.fromJson))
        let subrepresentation_ids = get_value("subrepresentations", j =>
          j->List.fromJson(Gid.fromJson)
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
                let id_get = (map, id) =>
                  Gid.Map.get(map, id)->Or_error.fromOption_ss([
                    "Unable to find value with ID '",
                    Gid.toString(id),
                    "'",
                  ])

                let tokens =
                  token_ids
                  ->Or_error.tag("Loading tokens")
                  ->Or_error.flatMap(token_ids =>
                    token_ids->List.map(id => id_get(tokens4, id))->Or_error.all
                  )
                let dimensions =
                  dimension_ids
                  ->Or_error.tag("Loading dimensions")
                  ->Or_error.flatMap(dimension_ids =>
                    dimension_ids->List.map(id => id_get(dimensions4, id))->Or_error.all
                  )
                let schemes =
                  scheme_ids
                  ->Or_error.tag("Loading schemes")
                  ->Or_error.flatMap(scheme_ids =>
                    scheme_ids->List.map(id => id_get(schemes4, id))->Or_error.all
                  )
                let subrepresentations =
                  subrepresentation_ids
                  ->Or_error.tag("Loading subrepresentations")
                  ->Or_error.flatMap(subrepresentation_ids =>
                    subrepresentation_ids
                    ->List.map(id => id_get(representations4, id))
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
                    id: id,
                    domain: domain,
                    display: display,
                    tokens: tokens,
                    dimensions: dimensions,
                    schemes: schemes,
                    subrepresentations: subrepresentations,
                  }
                  Or_error.create((
                    representations4->Gid.Map.set(id, t),
                    schemes4,
                    dimensions4,
                    tokens4,
                  ))->Or_error.tag(
                    Js.String2.concat(
                      "Successfully read Representation with ID ",
                      id->Gid.toString,
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
    ->Or_error.fromOption_s("JSON is not a valid object (reading Schema.Representation.t)")
    ->Or_error.flatMap(dict => {
      let id =
        dict
        ->Js.Dict.get("start")
        ->Or_error.fromOption_s("Unable to find start of model (reading Schema.Representation.t)")
        ->Or_error.flatMap(Gid.fromJson)
      id->Or_error.flatMap(id =>
        _fromJsonHelper(
          dict,
          id,
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
          Gid.Map.empty(),
        )->Or_error.flatMap(((representations, _, _, _)) =>
          representations
          ->Gid.Map.get(id)
          ->Or_error.fromOption_s("Missing start of model (reading Schema.Representation.t)")
        )
      )
    })
}
