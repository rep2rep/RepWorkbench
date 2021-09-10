include module type of Belt.Array

let toJson: (array<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => option<'a>) => option<array<'a>>
