let to_JSON: (option<'a>, 'a => Js.Json.t) => Js.Json.t
let of_JSON: (Js.Json.t, Js.Json.t => option<'a>) => option<option<'a>>
