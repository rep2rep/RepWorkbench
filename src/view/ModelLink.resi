module Payload: {
  type t

  let create: string => t
}

type t = ReactD3Graph.Link.t<Payload.t>

module Kind: {
  type t =
    | Heirarchy
    | Anchor
    | Relation
}

let create: (~source: ModelNode.t, ~target: ModelNode.t, Kind.t) => t
