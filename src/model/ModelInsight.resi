type t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let create: (~nodes: array<Gid.t>, ~message: string, ~details: string, unit) => t

let id: t => Gid.t
let nodes: t => array<Gid.t>
let message: t => string
let details: t => string
let suggestion: t => option<string> // Always None

let subsumes: (t, t) => bool
