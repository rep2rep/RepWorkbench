type t = ReactD3Graph.Graph.Selection.t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let empty: t

let nodes: t => array<ReactD3Graph.Node.Id.t>
let links: t => array<ReactD3Graph.Link.Id.t>
