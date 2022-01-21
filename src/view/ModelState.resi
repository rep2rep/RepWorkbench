type t

let load: unit => option<t>
let save: t => unit

let init: t
let data: t => ReactD3Graph.Data.t<ModelNode.Payload.t, ModelLink.Payload.t>

let addNode: (t, ModelNode.t) => t
let addLink: (t, ModelLink.t) => t
let removeNode: (t, ReactD3Graph.Node.Id.t) => t
let updateNodes: (t, ModelNode.t => ModelNode.t) => t
let nodeWithId: (t, ReactD3Graph.Node.Id.t) => option<ModelNode.t>

let selection: t => ModelSelection.t
let setSelection: (t, ModelSelection.t) => t
