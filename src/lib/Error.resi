type t

let raise_: t => 'a

let join: (t, t) => t
let concat: list<t> => t
let tag: (t, string) => t

let toString: t => string
let fromString: string => t
let fromStrings: array<string> => t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => option<t>

// Not for use
let _pretag: (t, array<string>) => t
let _posttag: (t, array<string>) => t
