type t = ReactD3Graph.Graph.Selection.t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let empty: t
let singleton: Uuid.t => t

let nodes: t => array<Uuid.t>
let links: t => array<ReactD3Graph.Link.Id.t>
