type t = Global | Local

let toJson = t =>
  switch t {
  | Global => String.toJson("Global")
  | Local => String.toJson("Local")
  }

let fromJson = json =>
  switch String.fromJson(json)->Or_error.valOf {
  | Some("Global") => Or_error.create(Global)
  | Some("Local") => Or_error.create(Local)
  | Some(_) => Or_error.error_s("Scope is not one of Global or Local")
  | None => Or_error.error_s("Unable to decode string (reading Scope.t)")
  }
