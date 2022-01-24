type t = Global | Local

let toString: t => string
let fromString: string => option<t>

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let all: array<t>
