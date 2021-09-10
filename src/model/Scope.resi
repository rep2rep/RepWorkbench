type t = Global | Local

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => option<t>
