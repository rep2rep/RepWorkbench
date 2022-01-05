type t

let init: t
let data: t => ReactD3Graph.Data.t<ModelNode.Payload.t, ModelLink.Payload.t>

let addNode: (t, ModelNode.t) => t
let addLink: (t, ModelLink.t) => t
let nodeWithId: (t, ReactD3Graph.Node.Id.t) => option<ModelNode.t>

let addToSelection: (t, ReactD3Graph.Node.Id.t) => t
let removeFromSelection: (t, ReactD3Graph.Node.Id.t) => t
let clearSelection: t => t
let nodeIsSelected: (t, ReactD3Graph.Node.Id.t) => bool
let selectedNodeIds: t => array<ReactD3Graph.Node.Id.t>
