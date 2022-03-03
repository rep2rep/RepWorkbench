type t<'a>

let toJson: (t<'a>, 'a => Js.Json.t) => Js.Json.t
let fromJson: (Js.Json.t, Js.Json.t => t<'a>) => t<t<'a>>

let toOption: t<'a> => option<'a>
let fromOption: (option<'a>, Error.t) => t<'a>
let fromOption_s: (option<'a>, string) => t<'a>
let fromOption_ss: (option<'a>, array<string>) => t<'a>

let toBool: t<unit> => bool
let fromBool: (bool, Error.t) => t<unit>
let fromBool_s: (bool, string) => t<unit>
let fromBool_ss: (bool, array<string>) => t<unit>

let toString: (t<'a>, 'a => string) => string

let create: 'a => t<'a>
let error: Error.t => t<'a>
let error_s: string => t<'a>
let error_ss: array<string> => t<'a>

let isOk: t<'a> => bool
let isError: t<'a> => bool

let tag: (t<'a>, string) => t<'a>
let tags: t<'a> => array<Error.Tag.t>

let messages: t<'a> => option<array<string>>

let okExn: t<'a> => 'a
let okExnWithTags: t<'a> => 'a
let valOf: t<'a> => option<'a>
let getWithDefault: (t<'a>, 'a) => 'a
let throwIfError: t<'a> => t<'a>

let map: (t<'a>, 'a => 'b) => t<'b>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
let flatten: t<t<'a>> => t<'a>

let all: list<t<'a>> => t<list<'a>>
let allUnit: list<t<unit>> => t<unit>
let both: ((t<'a>, t<'b>)) => t<('a, 'b)>
include Or_error_boths.S with type t<'a> := t<'a>
