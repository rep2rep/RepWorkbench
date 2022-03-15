module Kind: {
  type t =
    | Hierarchy
    | Anchor
    | Relation

  module Stable: {
    module V1: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

module Payload: {
  type t

  let create: Kind.t => t

  module Stable: {
    module V1: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

type t

let data: t => array<ReactD3Graph.Link.t<Payload.t>>

let create: (~source: ModelNode.t, ~target: ModelNode.t, Kind.t) => t

let source: t => Gid.t
let target: t => Gid.t
let kind: t => Kind.t

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
