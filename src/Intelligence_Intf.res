module Request = {
  type t = {
    id: Gid.t,
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
      ("slots", t.slots->Gid.Map.toJson(InspectorState.Schema.Stable.V2.toJson)),
      (
        "links",
        t.links->Array.toJson(
          tuple3ToJson(Gid.toJson, Gid.toJson, ModelLink.Kind.Stable.V1.toJson),
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
      let slots = getValue("slots", Gid.Map.fromJson(_, InspectorState.Schema.Stable.V2.fromJson))
      let links = getValue(
        "links",
        Array.fromJson(
          _,
          tuple3FromJson(Gid.fromJson, Gid.fromJson, ModelLink.Kind.Stable.V1.fromJson),
        ),
      )
      Or_error.both3((id, slots, links))->Or_error.map(((id, slots, links)) => {
        id: id,
        slots: slots,
        links: links,
      })
    })
}

module Response = {
  type t = {
    id: Gid.t,
    warnings: array<ModelWarning.t>,
    errors: array<ModelError.t>,
  }

  let toJson = t =>
    Js.Dict.fromList(list{
      ("id", t.id->Gid.toJson),
      ("warnings", t.warnings->Array.toJson(ModelWarning.toJson)),
      ("errors", t.errors->Array.toJson(ModelError.toJson)),
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
      let warnings = getValue("warnings", Array.fromJson(_, ModelWarning.fromJson))
      let errors = getValue("errors", Array.fromJson(_, ModelError.fromJson))

      Or_error.both3((id, warnings, errors))->Or_error.map(((id, warnings, errors)) => {
        id: id,
        warnings: warnings,
        errors: errors,
      })
    })

  let empty = {
    id: Gid.create(),
    warnings: [],
    errors: [],
  }
}

module T = Worker.Make(Request, Response)
include T
