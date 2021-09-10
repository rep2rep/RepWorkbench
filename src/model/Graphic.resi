type t = string

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => option<t>
