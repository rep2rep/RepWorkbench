include Belt.List

let empty = list{}

let mapPartial = (t, f) =>
  t->reduceReverse(list{}, (xs, x) =>
    switch f(x) {
    | None => xs
    | Some(x) => xs->add(x)
    }
  )

let toJson = (t, jsonify) => t->map(jsonify)->toArray->Js.Json.array

let fromJson = (json, decode) =>
  json
  ->Js.Json.decodeArray
  ->Option.flatMap(arr =>
    arr
    ->fromArray
    ->reduceReverse(Some(list{}), (xs, x) =>
      xs->Option.flatMap(xs => decode(x)->Option.map(x => xs->add(x)))
    )
  )
