type t = ReactD3Graph.Graph.Selection.t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let duplicate: (t, Uuid.Map.t<Uuid.t>) => t

let empty: t
let ofNodes: array<Uuid.t> => t

let nodes: t => array<Uuid.t>
let links: t => array<ReactD3Graph.Link.Id.t>
