type t

let duplicate: (t, Gid.Map.t<Gid.t>) => t

let empty: t
let data: t => ReactD3Graph.Data.t<ModelNode.Payload.t, ModelLink.Payload.t>

let hash: t => Hash.t

let graph: t => ModelGraph.t
let addNode: (t, ModelNode.t) => t
let addNodes: (t, array<ModelNode.t>) => t
let duplicateNodes: (t, Gid.Map.t<Gid.t>) => t
let addLink: (t, ModelLink.t) => t
let addLinks: (t, array<ModelLink.t>) => t
let removeNode: (t, Gid.t) => t
let removeLink: (t, Gid.t) => t
let moveNode: (t, Gid.t, ~x: float, ~y: float) => t
let updateNodes: (t, ModelNode.t => ModelNode.t) => t
let nodeWithId: (t, Gid.t) => option<ModelNode.t>
let incidentLinks: (t, ~nodeId: Gid.t) => {"incoming": Gid.Set.t, "outgoing": Gid.Set.t}
let linksConnectingNodes: (t, array<Gid.t>) => array<Gid.t>

let selection: t => ModelSelection.t
let setSelection: (t, ModelSelection.t) => t

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
    let v1_to_v2: V1.t => t
  }

  module V3: {
    type t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
    let v2_to_v3: V2.t => t
  }

  module V4: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
    let v3_to_v4: V3.t => t
  }
}
