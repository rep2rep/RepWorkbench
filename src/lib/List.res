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

let isEmpty = t =>
  switch t {
  | list{} => true
  | _ => false
  }

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
  json
  ->Js.Json.decodeArray
  ->Or_error.fromOption_s("JSON is not a valid array (reading list)")
  ->Or_error.flatMap(arr => arr->fromArray->map(decode)->Or_error.all)

let toString = (t, stringify) =>
  t->map(stringify)->reduce("list{", (s, a) => String.concatMany(s, [a, ", "]))->String.concat("}")
