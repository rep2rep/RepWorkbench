include Belt.Array

let toJson = (t, jsonify) => t->map(jsonify)->Js.Json.array
let fromJson = (j, decode) =>
  j
  ->Js.Json.decodeArray
  ->Option.flatMap(arr =>
    arr->reduce(Some([]), (xs, x) =>
      xs->Option.flatMap(xs => decode(x)->Option.map(x => concat(xs, [x])))
    )
  )
