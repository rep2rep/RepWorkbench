include Belt.Option

let some = x => Some(x)
let flatten = tt => flatMap(tt, t => t)
let iter = (x, f) =>
  switch x {
  | Some(x) => f(x)
  | None => ()
  }

let toJson = (t, jsonify) =>
  switch t {
  | None => Js.Json.null
  | Some(x) => jsonify(x)
  }

let fromJson = (json, decode) =>
  if json === Js.Json.null {
    Or_error.create(None)
  } else {
    decode(json)->Or_error.map(x => Some(x))
  }
