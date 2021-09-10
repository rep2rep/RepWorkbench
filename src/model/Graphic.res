type t = string

let toJson = Js.Json.string
let fromJson = Js.Json.decodeString
