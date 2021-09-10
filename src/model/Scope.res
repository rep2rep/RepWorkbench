type t = Global | Local

let toJson = t =>
  switch t {
  | Global => Js.Json.string("Global")
  | Local => Js.Json.string("Local")
  }

let fromJson = json =>
  switch Js.Json.decodeString(json) {
  | Some("Global") => Some(Global)
  | Some("Local") => Some(Local)
  | Some(_) => None
  | None => None
  }
