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
    ->Option.flatMap(json =>
      Js.Json.decodeObject(json)->Option.flatMap(dict => {
        let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)

        let domain = get_value("domain", String.fromJson)
        let display = get_value("display", Graphic.fromJson)

        let token_ids = get_value("tokens", j => j->List.fromJson(Uuid.fromJson))
        let dimension_ids = get_value("dimensions", j => j->List.fromJson(Uuid.fromJson))
        let scheme_ids = get_value("schemes", j => j->List.fromJson(Uuid.fromJson))
        let subrepresentation_ids = get_value("subrepresentations", j =>
          j->List.fromJson(Uuid.fromJson)
        )

        let (
          (tokens, dimensions, schemes, subrepresentations),
          (representations1, schemes1, dimensions1, tokens1),
        ) =
          token_ids
          ->Schema_intf.recurse(
            Token._fromJsonHelper,
            global_dict,
            Schema_intf.Token,
            representations0,
            schemes0,
            dimensions0,
            tokens0,
          )
          ->Option.flatMap(((representations1, schemes1, dimensions1, tokens1)) =>
            dimension_ids
            ->Schema_intf.recurse(
              Dimension._fromJsonHelper,
              global_dict,
              Schema_intf.Dimension,
              representations1,
              schemes1,
              dimensions1,
              tokens1,
            )
            ->Option.flatMap(((representations2, schemes2, dimensions2, tokens2)) =>
              scheme_ids
              ->Schema_intf.recurse(
                Scheme._fromJsonHelper,
                global_dict,
                Schema_intf.Scheme,
                representations2,
                schemes2,
                dimensions2,
                tokens2,
              )
              ->Option.flatMap(((representations3, schemes3, dimensions3, tokens3)) =>
                subrepresentation_ids
                ->Schema_intf.recurse(
                  _fromJsonHelper,
                  global_dict,
                  Schema_intf.Representation,
                  representations3,
                  schemes3,
                  dimensions3,
                  tokens3,
                )
                ->Option.flatMap(((representations4, schemes4, dimensions4, tokens4)) => {
                  let tokens =
                    token_ids->Option.flatMap(token_ids =>
                      token_ids->List.map(uuid => Uuid.Map.get(tokens4, uuid))->List.allSome
                    )
                  let dimensions =
                    dimension_ids->Option.flatMap(dimension_ids =>
                      dimension_ids->List.map(uuid => Uuid.Map.get(dimensions4, uuid))->List.allSome
                    )
                  let schemes =
                    scheme_ids->Option.flatMap(scheme_ids =>
                      scheme_ids->List.map(uuid => Uuid.Map.get(schemes4, uuid))->List.allSome
                    )
                  let subrepresentations =
                    subrepresentation_ids->Option.flatMap(subrepresentation_ids =>
                      subrepresentation_ids
                      ->List.map(uuid => Uuid.Map.get(representations4, uuid))
                      ->List.allSome
                    )

                  Some((
                    (tokens, dimensions, schemes, subrepresentations),
                    (representations4, schemes4, dimensions4, tokens4),
                  ))
                })
              )
            )
          )
          ->Option.getExn

        switch (domain, display, tokens, dimensions, schemes, subrepresentations) {
        | (
            Some(domain),
            Some(display),
            Some(tokens),
            Some(dimensions),
            Some(schemes),
            Some(subrepresentations),
          ) => {
            let t = {
              uuid: uuid,
              domain: domain,
              display: display,
              tokens: tokens,
              dimensions: dimensions,
              schemes: schemes,
              subrepresentations: subrepresentations,
            }
            Some((representations1->Uuid.Map.set(uuid, t), schemes1, dimensions1, tokens1))
          }
        | _ => None
        }
      })
    )
  }

  let fromJson = json =>
    Js.Json.decodeObject(json)->Option.flatMap(dict => {
      let uuid = dict->Js.Dict.get("start")->Option.flatMap(Uuid.fromJson)
      uuid->Option.flatMap(uuid =>
        _fromJsonHelper(
          dict,
          uuid,
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
          Uuid.Map.empty(),
        )->Option.flatMap(((representations, _, _, _)) => representations->Uuid.Map.get(uuid))
      )
    })
}
