include Belt.List

let to_JSON = (t, jsonify) => t->map(jsonify)->toArray->Js.Json.array

let of_JSON = (json, decode) =>
  json
  ->Js.Json.decodeArray
  ->Option.flatMap(arr =>
    arr
    ->fromArray
    ->reduce(Some(list{}), (result, x) =>
      switch result {
      | None => None
      | Some(xs) =>
        switch decode(x) {
        | None => None
        | Some(x) => Some(xs->add(x))
        }
      }
    )
  )
