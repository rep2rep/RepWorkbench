type t

let load: string => option<t>
let save: (string, t) => unit

let empty: t
let data: t => ReactD3Graph.Data.t<ModelNode.Payload.t, ModelLink.Payload.t>

let addNode: (t, ModelNode.t) => t
let addLink: (t, ModelLink.t) => t
let removeNode: (t, Uuid.t) => t
let updateNodes: (t, ModelNode.t => ModelNode.t) => t
let nodeWithId: (t, Uuid.t) => option<ModelNode.t>

let selection: t => ModelSelection.t
let setSelection: (t, ModelSelection.t) => t
