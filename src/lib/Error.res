type t = Js.Array2.t<Js.String2.t>

let join = Js.Array2.concat
let concat = ts => Belt.List.reduce(ts, [], join)

let toString = t => Js.String2.concatMany("", t)
let fromString = s => [s]
let fromStrings = ss => ss

let raise_ = t => Js.Exn.raiseError(toString(t))

let toJson = t => toString(t)->Js.Json.string
let fromJson = json => json->Js.Json.decodeString->Belt.Option.map(fromString)
