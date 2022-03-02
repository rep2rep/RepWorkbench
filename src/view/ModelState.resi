type t

let duplicate: (t, Uuid.Map.t<Uuid.t>) => t

let empty: t
let data: t => ReactD3Graph.Data.t<ModelNode.Payload.t, ModelLink.Payload.t>

let graph: t => ModelGraph.t
let addNode: (t, ModelNode.t) => t
let addNodes: (t, array<ModelNode.t>) => t
let duplicateNodes: (t, Uuid.Map.t<Uuid.t>) => t
let addLink: (t, ModelLink.t) => t
let addLinks: (t, array<ModelLink.t>) => t
let removeNode: (t, Uuid.t) => t
let removeLinks: (t, array<ModelLink.t>) => t
let moveNode: (t, Uuid.t, ~x: float, ~y: float) => t
let updateNodes: (t, ModelNode.t => ModelNode.t) => t
let nodeWithId: (t, Uuid.t) => option<ModelNode.t>

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
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
    let v2_to_v3: V2.t => t
  }
}
