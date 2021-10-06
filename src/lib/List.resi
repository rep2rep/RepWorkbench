include module type of Belt.List

let toJson: (list<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => Or_error.t<'a>) => Or_error.t<list<'a>>

let empty: list<'a>
let singleton: 'a => list<'a>
let range: int => list<int>
let mapPartial: (list<'a>, 'a => option<'b>) => list<'b>
let allSome: list<option<'a>> => option<list<'a>>
