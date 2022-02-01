module Kind: {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token
}

module Payload: {
  type t = {
    kind: Kind.t,
    name: string,
    name_suffix: option<string>,
    reference: string,
    reference_suffix: option<string>,
    dashed: bool,
  }

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

type t

let data: t => array<ReactD3Graph.Node.t<Payload.t>>

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let create: (~name: string, ~reference: string, ~x: float, ~y: float, Kind.t, Uuid.t) => t
let dupWithNewId: (t, Uuid.t) => t
let id: t => Uuid.t
let kind: t => Kind.t
let position: t => (float, float)
let setPosition: (t, ~x: float, ~y: float) => t
let updatePayload: (t, Payload.t => Payload.t) => t
