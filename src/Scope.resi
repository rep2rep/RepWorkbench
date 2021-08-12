type t = Global | Local

let to_JSON: t => Js.Json.t
let of_JSON: Js.Json.t => option<t>

let jsx: t => React.element
