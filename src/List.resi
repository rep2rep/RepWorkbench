include module type of Belt.List

let to_JSON: (list<'a>, 'a => Js.Json.t) => Js.Json.t
let of_JSON: (Js.Json.t, Js.Json.t => option<'a>) => option<list<'a>>

let jsx: (list<'a>, 'a => React.element) => React.element
