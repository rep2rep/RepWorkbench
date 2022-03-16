type t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let create: (~node: Gid.t, ~message: string, ~details: string) => t

let id: t => Gid.t
let node: t => Gid.t
let message: t => string
let details: t => string
