include module type of Belt.List

let toJson: (list<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => option<'a>) => option<list<'a>>

let empty: list<'a>
let mapPartial: (list<'a>, 'a => option<'b>) => list<'b>
