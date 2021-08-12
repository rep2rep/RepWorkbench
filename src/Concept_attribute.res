type t = string

let to_JSON = Js.Json.string
let of_JSON = Js.Json.decodeString

let jsx = React.string
