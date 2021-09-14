type t = bool

let not = t => !t

let toJson = Js.Json.boolean
let fromJson = Js.Json.decodeBoolean
