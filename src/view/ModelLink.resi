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
  let setLabel: (t, option<int>) => t

  module Stable: {
    module V1: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V2: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V3: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

type t

let hash: t => Hash.t
let data: t => array<ReactD3Graph.Link.t<Payload.t>>

let create: (
  ~linkId: Gid.t,
  ~source: ModelNode.t,
  ~target: ModelNode.t,
  Kind.t,
  ~label: option<int>,
) => t

let id: t => Gid.t
let source: t => Gid.t
let target: t => Gid.t
let kind: t => Kind.t
let label: t => option<int>
let updatePayload: (t, Payload.t => Payload.t) => t

module Stable: {
  module V1: {
    type t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }

  module V2: {
    type t
    let v1_to_v2: V1.t => t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>

    let id: t => Gid.t
    let kind: t => Kind.Stable.V2.t
  }

  module V3: {
    type t = t
    let v2_to_v3: V2.t => t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
