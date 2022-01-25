type t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let empty: t
let data: t => ReactD3Graph.Data.t<ModelNode.Payload.t, ModelLink.Payload.t>

let graph: t => ModelGraph.t
let addNode: (t, ModelNode.t) => t
let addLink: (t, ModelLink.t) => t
let removeNode: (t, Uuid.t) => t
let removeLinks: (t, array<ModelLink.t>) => t
let updateNodes: (t, ModelNode.t => ModelNode.t) => t
let nodeWithId: (t, Uuid.t) => option<ModelNode.t>

let selection: t => ModelSelection.t
let setSelection: (t, ModelSelection.t) => t
