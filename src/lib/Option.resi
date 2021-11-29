include module type of Belt.Option

let toJson: (option<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => Or_error.t<'a>) => Or_error.t<option<'a>>

let some: 'a => option<'a>
let flatten: option<option<'a>> => option<'a>
let iter: (option<'a>, 'a => unit) => unit
