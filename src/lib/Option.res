include Belt.Option

let some = x => Some(x)
let flatten = tt => flatMap(tt, t => t)
let iter = (x, f) =>
  switch x {
  | Some(x) => f(x)
  | None => ()
  }
let both = ((a, b)) => a->flatMap(a => b->map(b => (a, b)))
let both3 = ((a, b, c)) => a->flatMap(a => b->flatMap(b => c->map(c => (a, b, c))))
let all = xs =>
  xs->Array.reduce(Some([]), (ys, x) => both((ys, x))->map(((ys, x)) => Array.concat(ys, [x])))

let toJson = (t, jsonify) =>
  switch t {
  | None => Js.Json.null
  | Some(x) => jsonify(x)
  }

let fromJson = (json, decode) =>
  if json === Js.Json.null {
    Or_error.create(None)
  } else {
    decode(json)->Or_error.map(x => Some(x))
  }
