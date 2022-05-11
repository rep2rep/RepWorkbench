module Kind: {
  type t =
    | Hierarchy
    | Anchor
    | Relation
    | Overlap
    | Disjoint
    | Generic

  module Stable: {
    module V1: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V2: {
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
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V2: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

type t

let data: t => array<ReactD3Graph.Link.t<Payload.t>>

let create: (~linkId: Gid.t, ~source: ModelNode.t, ~target: ModelNode.t, Kind.t) => t

let id: t => Gid.t
let source: t => Gid.t
let target: t => Gid.t
let kind: t => Kind.t

module Stable: {
  module V1: {
    type t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }

  module V2: {
    type t = t
    let v1_to_v2: V1.t => t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
