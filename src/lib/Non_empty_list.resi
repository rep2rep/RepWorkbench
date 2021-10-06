type t<'a>

let toJson: (t<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => Or_error.t<'a>) => Or_error.t<t<'a>>

let toList: t<'a> => list<'a>
let fromList: list<'a> => option<t<'a>>

let toArray: t<'a> => array<'a>

let singleton: 'a => t<'a>
let cons: (t<'a>, 'a) => t<'a>
let head: t<'a> => 'a
let length: t<'a> => int
let reverse: t<'a> => t<'a>
let map: (t<'a>, 'a => 'b) => t<'b>
let reduce': (t<'a>, ('a, 'a) => 'a) => 'a
let reduce: (t<'a>, 'a => 'b, ('b, 'a) => 'b) => 'b
let allSome: t<option<'a>> => option<t<'a>>
let foldr: (t<'a>, 'b, ('a, 'b) => 'b) => 'b
let every: (t<'a>, 'a => bool) => bool

let or_error_all: t<Or_error.t<'a>> => Or_error.t<t<'a>>
