type t<'a> =
  | Ok('a)
  | Err(Error.t)

let toJson: (t<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => t<'a>) => t<t<'a>>

let toOption: t<'a> => option<'a>
let fromOption: (option<'a>, Error.t) => t<'a>

let toString: (t<'a>, 'a => string) => string

let create: 'a => t<'a>
let error: Error.t => t<'a>
let error_s: string => t<'a>

let okExn: t<'a> => 'a
let valOf: t<'a> => option<'a>
let getWithDefault: (t<'a>, 'a) => 'a

let map: (t<'a>, 'a => 'b) => t<'b>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

let all: list<t<'a>> => t<list<'a>>
let both: ((t<'a>, t<'b>)) => t<('a, 'b)>
include Or_error_boths.S with type t<'a> := t<'a>
