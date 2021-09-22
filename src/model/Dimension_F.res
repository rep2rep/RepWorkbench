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
    t.concept !== "" &&
    t.explicit === !Option.isNone(t.graphic) &&
    t.dimensions->List.every(validate) &&
    t.tokens->Non_empty_list.every(Token.validate)

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
    ->Option.flatMap(json =>
      Js.Json.decodeObject(json)->Option.flatMap(dict => {
        let get_value = (key, decode) => dict->Js.Dict.get(key)->Option.flatMap(decode)

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

        let ((dimensions, tokens), (representations1, schemes1, dimensions1, tokens1)) =
          dimension_ids
          ->Schema_intf.recurse(
            _fromJsonHelper,
            global_dict,
            Schema_intf.Dimension,
            representations0,
            schemes0,
            dimensions0,
            tokens0,
          )
          ->Option.flatMap(((representations1, schemes1, dimensions1, tokens1)) =>
            token_ids
            ->Option.map(Non_empty_list.toList)
            ->Schema_intf.recurse(
              Token._fromJsonHelper,
              global_dict,
              Schema_intf.Token,
              representations1,
              schemes1,
              dimensions1,
              tokens1,
            )
            ->Option.flatMap(((representations2, schemes2, dimensions2, tokens2)) => {
              let dimensions =
                dimension_ids->Option.flatMap(dimension_ids =>
                  dimension_ids->List.map(uuid => dimensions2->Uuid.Map.get(uuid))->List.allSome
                )
              let tokens =
                token_ids->Option.flatMap(token_ids =>
                  token_ids
                  ->Non_empty_list.map(uuid => tokens2->Uuid.Map.get(uuid))
                  ->Non_empty_list.allSome
                )

              Some(((dimensions, tokens), (representations2, schemes2, dimensions2, tokens2)))
            })
          )
          ->Option.getExn

        switch (
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
        ) {
        | (
            Some(concept),
            Some(concept_scale),
            Some(concept_type),
            Some(concept_attributes),
            Some(graphic),
            Some(graphic_scale),
            Some(graphic_type),
            Some(graphic_attributes),
            Some(function),
            Some(scope),
            Some(explicit),
            Some(dimensions),
            Some(tokens),
          ) => {
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

            Some((representations1, schemes1, dimensions1->Uuid.Map.set(uuid, t), tokens1))
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
        )->Option.flatMap(((_, _, dimensions, _)) => dimensions->Uuid.Map.get(uuid))
      )
    })
}
