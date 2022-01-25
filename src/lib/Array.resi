include module type of Belt.Array

let toJson: (array<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => Or_error.t<'a>) => Or_error.t<array<'a>>

let joinWith: (array<string>, string) => string
let filter: (array<'a>, 'a => bool) => array<'a>
let find: (array<'a>, 'a => bool) => option<'a>
let includes: (array<'a>, 'a) => bool
let flatMap: (array<'a>, 'a => array<'b>) => array<'b>
