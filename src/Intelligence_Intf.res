module Request = {
  type t = {
    id: Gid.t,
    model: Gid.t,
    slots: Gid.Map.t<InspectorState.Schema.t>,
    links: array<(Gid.t, Gid.t, ModelLink.Kind.t)>,
  }

  let tuple3ToJson = (ajson, bjson, cjson, (a, b, c)) =>
    [ajson(a), bjson(b), cjson(c)]->Js.Json.array
  let tuple3FromJson = (ajson, bjson, cjson, json) =>
    switch Js.Json.decodeArray(json)->Option.getWithDefault([]) {
    | [a, b, c] => (ajson(a), bjson(b), cjson(c))->Or_error.both3
    | _ => Or_error.error_s("Not a triple")
    }

  let toJson = t =>
    Js.Dict.fromList(list{
      ("id", t.id->Gid.toJson),
      ("model", t.model->Gid.toJson),
      ("slots", t.slots->Gid.Map.toJson(InspectorState.Schema.Stable.V2.toJson)),
      (
        "links",
        t.links->Array.toJson(
          tuple3ToJson(Gid.toJson, Gid.toJson, ModelLink.Kind.Stable.V2.toJson),
        ),
      ),
    })->Js.Json.object_

  let fromJson = json =>
    json
    ->Js.Json.decodeObject
    ->Or_error.fromOption_s("Failed to decode Request state object JSON")
    ->Or_error.flatMap(dict => {
      let getValue = (key, reader) =>
        dict
        ->Js.Dict.get(key)
        ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
        ->Or_error.flatMap(reader)
      let id = getValue("id", Gid.fromJson)
      let model = getValue("model", Gid.fromJson)
      let slots = getValue("slots", Gid.Map.fromJson(_, InspectorState.Schema.Stable.V2.fromJson))
      let links = getValue(
        "links",
        Array.fromJson(
          _,
          tuple3FromJson(Gid.fromJson, Gid.fromJson, ModelLink.Kind.Stable.V2.fromJson),
        ),
      )
      Or_error.both4((id, model, slots, links))->Or_error.map(((id, model, slots, links)) => {
        id: id,
        model: model,
        slots: slots,
        links: links,
      })
    })
}

module Response = {
  type t = {
    id: Gid.t,
    model: Gid.t,
    warnings: array<ModelWarning.t>,
    warnings_done: bool,
    errors: array<ModelError.t>,
    errors_done: bool,
    insights: array<ModelInsight.t>,
    insights_done: bool,
  }

  let toJson = t =>
    Js.Dict.fromList(list{
      ("id", t.id->Gid.toJson),
      ("model", t.model->Gid.toJson),
      ("warnings", t.warnings->Array.toJson(ModelWarning.toJson)),
      ("warnings_done", t.warnings_done->Bool.toJson),
      ("errors", t.errors->Array.toJson(ModelError.toJson)),
      ("errors_done", t.errors_done->Bool.toJson),
      ("insights", t.insights->Array.toJson(ModelInsight.toJson)),
      ("insights_done", t.insights_done->Bool.toJson),
    })->Js.Json.object_

  let fromJson = json =>
    json
    ->Js.Json.decodeObject
    ->Or_error.fromOption_s("Failed to decode Response state object JSON")
    ->Or_error.flatMap(dict => {
      let getValue = (key, reader) =>
        dict
        ->Js.Dict.get(key)
        ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
        ->Or_error.flatMap(reader)
      let id = getValue("id", Gid.fromJson)
      let model = getValue("model", Gid.fromJson)
      let warnings = getValue("warnings", Array.fromJson(_, ModelWarning.fromJson))
      let warnings_done = getValue("warnings_done", Bool.fromJson)
      let errors = getValue("errors", Array.fromJson(_, ModelError.fromJson))
      let errors_done = getValue("errors_done", Bool.fromJson)
      let insights = getValue("insights", Array.fromJson(_, ModelInsight.fromJson))
      let insights_done = getValue("insights_done", Bool.fromJson)

      Or_error.both8((
        id,
        model,
        warnings,
        warnings_done,
        errors,
        errors_done,
        insights,
        insights_done,
      ))->Or_error.map(((
        id,
        model,
        warnings,
        warnings_done,
        errors,
        errors_done,
        insights,
        insights_done,
      )) => {
        id: id,
        model: model,
        warnings: warnings,
        warnings_done: warnings_done,
        errors: errors,
        errors_done: errors_done,
        insights: insights,
        insights_done: insights_done,
      })
    })

  let empty = {
    id: Gid.create(),
    model: Gid.create(),
    warnings: [],
    warnings_done: true,
    errors: [],
    errors_done: true,
    insights: [],
    insights_done: true,
  }
}

module T = Worker.Make(Request, Response)
include T
