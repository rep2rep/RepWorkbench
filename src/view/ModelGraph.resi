type t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

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
