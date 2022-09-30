type t

let hash: t => Hash.t
let duplicate: (t, Gid.Map.t<Gid.t>) => t

let empty: t

let nodes: t => array<ModelNode.t>
let links: t => array<ModelLink.t>

let addNodes: (t, array<ModelNode.t>) => t
let addNode: (t, ModelNode.t) => t
let removeNode: (t, Gid.t) => t
let mapNodes: (t, ModelNode.t => ModelNode.t) => t

let addLinks: (t, array<ModelLink.t>) => t
let addLink: (t, ModelLink.t) => t
let removeLink: (t, Gid.t) => t
let mapLinks: (t, ModelLink.t => ModelLink.t) => t

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
  }
  module V3: {
    type t
    let v2_to_v3: V2.t => t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>

    let links: t => array<ModelLink.Stable.V2.t>
  }
  module V4: {
    type t = t
    let v3_to_v4: V3.t => t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
