include module type of Belt.Array

let toJson: (array<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => Or_error.t<'a>) => Or_error.t<array<'a>>

let joinWith: (array<string>, string) => string
