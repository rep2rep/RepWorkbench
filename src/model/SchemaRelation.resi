module Kind: {
  type t =
    | Identical
    | Equivalent
    | Parallel
    | Other(string)

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

type t

let toJson: t => Js.Json.t
let fromJson: (Js.Json.t, Schema.t) => Or_error.t<t>

let schema: t => (Schema.t, Schema.t)
let kind: t => Kind.t
