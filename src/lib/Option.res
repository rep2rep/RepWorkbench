include Belt.Option

let some = x => Some(x)

let to_JSON = (t, jsonify) =>
  switch t {
  | None => Js.Json.null
  | Some(x) => jsonify(x)
  }

let of_JSON = (json, decode) =>
  if json === Js.Json.null {
    Some(None)
  } else {
    decode(json)->map(x => Some(x))
  }
