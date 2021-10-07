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
