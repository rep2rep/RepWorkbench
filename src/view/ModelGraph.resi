type t

let duplicate: (t, Uuid.Map.t<Uuid.t>) => t

let empty: t

let nodes: t => array<ModelNode.t>
let links: t => array<ModelLink.t>

let addNodes: (t, array<ModelNode.t>) => t
let addNode: (t, ModelNode.t) => t
let removeNode: (t, Uuid.t) => t
let mapNodes: (t, ModelNode.t => ModelNode.t) => t

let addLinks: (t, array<ModelLink.t>) => t
let addLink: (t, ModelLink.t) => t
let removeLinks: (t, array<ModelLink.t>) => t

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
