let to_JSON = (t, jsonify) => t->List.map(jsonify)->List.toArray->Js.Json.array

let of_JSON = (json, decode) =>
  json
  ->Js.Json.decodeArray
  ->Option.flatMap(arr =>
    arr
    ->List.fromArray
    ->List.reduce(Some(list{}), (result, x) =>
      switch result {
      | None => None
      | Some(xs) =>
        switch decode(x) {
        | None => None
        | Some(x) => Some(xs->List.add(x))
        }
      }
    )
  )
