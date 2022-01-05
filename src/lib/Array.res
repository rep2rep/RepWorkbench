include Belt.Array

let toJson = (t, jsonify) => t->map(jsonify)->Js.Json.array
let fromJson = (j, decode) =>
  j
  ->Js.Json.decodeArray
  ->Or_error.fromOption_s("JSON is not a valid array (reading array)")
  ->Or_error.flatMap(arr =>
    arr->reduce(Or_error.create([]), (xs, x) =>
      xs->Or_error.flatMap(xs => decode(x)->Or_error.map(x => concat(xs, [x])))
    )
  )

let joinWith = Js.Array2.joinWith
let filter = Js.Array2.filter
let includes = Js.Array2.includes
let flatMap = (t, f) => Js.Array2.map(t, f) |> Js.Array2.concatMany([])
