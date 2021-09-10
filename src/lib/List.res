include Belt.List

let empty = list{}

let mapPartial = (t, f) =>
  t->reduceReverse(list{}, (xs, x) =>
    switch f(x) {
    | None => xs
    | Some(x) => xs->add(x)
    }
  )

let to_JSON = (t, jsonify) => t->map(jsonify)->toArray->Js.Json.array

let of_JSON = (json, decode) =>
  json->Js.Json.decodeArray->Option.map(arr => arr->fromArray->mapPartial(decode))
