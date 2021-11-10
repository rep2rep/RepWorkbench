module NodePayload = {
  type t = string

  let create = s => s
}

module LinkPayload = {
  type t = string

  let create = s => s
}

module Graph = {
  type t = {
    nodes: array<ReactD3Graph.Node.t<NodePayload.t>>,
    links: array<ReactD3Graph.Link.t<LinkPayload.t>>,
  }

  let empty = {
    nodes: [],
    links: [],
  }

  let addNode = (t, node) => {
    ...t,
    nodes: Js.Array.concat(t.nodes, [node]),
  }
}

module Selection = {
  type t = {
    nodes: array<ReactD3Graph.Node.t<NodePayload.t>>,
    links: array<ReactD3Graph.Link.t<LinkPayload.t>>,
  }

  let empty = {
    nodes: [],
    links: [],
  }
}

type t = {
  graph: Graph.t,
  selection: Selection.t,
}

let init = {
  graph: Graph.empty,
  selection: Selection.empty,
}

let data = t => {
  ReactD3Graph.Data.nodes: t.graph.nodes,
  links: t.graph.links,
}
let addNode = (t, node) => {...t, graph: t.graph->Graph.addNode(node)}
