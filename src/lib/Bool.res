type t = bool

let not = t => !t

let toJson = Js.Json.boolean
let fromJson = json =>
  json->Js.Json.decodeBoolean->Or_error.fromOption(Error.fromString("JSON is not a valid boolean"))
