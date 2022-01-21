module Kind: {
  type t =
    | Hierarchy
    | Anchor
    | Relation
}

module Payload: {
  type t

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>

  let create: Kind.t => t
}

type t

let data: t => array<ReactD3Graph.Link.t<Payload.t>>

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let create: (~source: ModelNode.t, ~target: ModelNode.t, Kind.t) => t

let source: t => ReactD3Graph.Node.Id.t
let target: t => ReactD3Graph.Node.Id.t
