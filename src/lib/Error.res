type t = Js.Array2.t<Js.String2.t>

let join = Js.Array2.concat
let concat = ts => Belt.List.reduce(ts, [], join)
let tag = (t, s) => join(t, [Js.String2.concat("TAG: ", s)])

let toString = t => Js.String2.concatMany("\n", t->Js.Array2.map(s => s->Js.String2.concat("\n")))
let fromString = s => [s]
let fromStrings = ss => [Js.String2.concatMany("", ss)]

let raise_ = t => Js.Exn.raiseError(toString(t))

let toJson = t => toString(t)->Js.Json.string
let fromJson = json => json->Js.Json.decodeString->Belt.Option.map(fromString)

let _pretag = (t, tags) => join(tags, t)
let _posttag = (t, tags) => join(t, tags)
