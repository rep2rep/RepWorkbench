include Belt.List

let range = len => {
  let rec helper = len' =>
    switch len' {
    | 0 => list{}
    | n => list{n - 1, ...helper(len' - 1)}
    }
  reverse(helper(len))
}

let empty = list{}

let singleton = a => list{a}

let mapPartial = (t, f) =>
  t->reduceReverse(list{}, (xs, x) =>
    switch f(x) {
    | None => xs
    | Some(x) => xs->add(x)
    }
  )

let allSome = t => {
  t->reduceReverse(Some(list{}), (xs, x) =>
    xs->Option.flatMap(xs => x->Belt.Option.map(x => xs->add(x)))
  )
}

let toJson = (t, jsonify) => t->map(jsonify)->toArray->Js.Json.array

let fromJson = (json, decode) =>
  json->Js.Json.decodeArray->Option.flatMap(arr => arr->fromArray->map(decode)->allSome)
