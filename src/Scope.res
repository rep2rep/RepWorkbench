type t = Global | Local

let to_JSON = t =>
  switch t {
  | Global => Js.Json.string("Global")
  | Local => Js.Json.string("Local")
  }

let of_JSON = json =>
  switch Js.Json.decodeString(json) {
  | Some("Global") => Some(Global)
  | Some("Local") => Some(Local)
  | Some(_) => None
  | None => None
  }
