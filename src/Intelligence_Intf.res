module Request = {
  // type t = {
  // slots: Uuid.Map.t<InspectorState.Schema.t>,
  // links: array<(Uuid.t, Uuid.t, ModelLink.Kind.t)>,
  // }
  type t = State.Model.t
  let toJson = State.Model.Stable.V3.toJson
  let fromJson = State.Model.Stable.V3.fromJson
}

module Response = {
  type t = {
    warnings: array<ModelWarning.t>,
    errors: array<ModelError.t>,
  }

  let toJson = t =>
    Js.Dict.fromList(list{
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
      let warnings = getValue("warnings", Array.fromJson(_, ModelWarning.fromJson))
      let errors = getValue("errors", Array.fromJson(_, ModelError.fromJson))

      Or_error.both((warnings, errors))->Or_error.map(((warnings, errors)) => {
        warnings: warnings,
        errors: errors,
      })
    })
}

module T = Worker.Make(Request, Response)
include T
