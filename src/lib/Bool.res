type t = bool

let not = t => !t

let toString = t =>
  if t {
    "true"
  } else {
    "false"
  }

let fromString = s =>
  switch s {
  | "true" => Some(true)
  | "false" => Some(false)
  | _ => None
  }

let toJson = Js.Json.boolean
let fromJson = json =>
  json->Js.Json.decodeBoolean->Or_error.fromOption_s("JSON is not a valid boolean")
