include module type of Belt.Option

let toJson: (option<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => option<'a>) => option<option<'a>>

let some: 'a => option<'a>
