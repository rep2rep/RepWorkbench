module Tag: {
  type t
  let toString: t => string
  let fromString: string => t
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
}

type t

let raise_: t => 'a
let raiseWithTags: t => 'a

let join: (t, t) => t
let concat: list<t> => t

let tag: (t, string) => t
let tags: t => array<Tag.t>
let messages: t => array<string>

let toString: t => string
let fromString: string => t
let fromStrings: array<string> => t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => option<t>

// Not for use
let _pretag: (t, array<Tag.t>) => t
let _posttag: (t, array<Tag.t>) => t
