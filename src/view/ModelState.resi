module NodePayload: {
  type t = {
    name: string,
    reference: string,
  }

  let create: (string, string) => t
}

module LinkPayload: {
  type t

  let create: string => t
}

module Graph: {
  type t = {
    nodes: array<ReactD3Graph.Node.t<NodePayload.t>>,
    links: array<ReactD3Graph.Link.t<LinkPayload.t>>,
  }
}

module Selection: {
  type t = {
    nodes: array<ReactD3Graph.Node.t<NodePayload.t>>,
    links: array<ReactD3Graph.Link.t<LinkPayload.t>>,
  }
}

type t = {
  graph: Graph.t,
  selection: Selection.t,
}

let init: t
let data: t => ReactD3Graph.Data.t<NodePayload.t, LinkPayload.t>
let addNode: (t, ReactD3Graph.Node.t<NodePayload.t>) => t
