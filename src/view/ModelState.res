module Graph = {
  type t = {
    nodes: array<ModelNode.t>,
    links: array<ModelLink.t>,
  }

  let empty = {
    nodes: [],
    links: [],
  }

  let addNodes = (t, nodes) => {
    ...t,
    nodes: Array.concat(t.nodes, nodes),
  }
  let addNode = (t, node) => addNodes(t, [node])

  let removeNode = (t, nodeId) => {
    ...t,
    nodes: t.nodes->Array.filter(node => ModelNode.id(node) != nodeId),
    links: t.links->Array.filter(link =>
      ModelLink.source(link) != nodeId && ModelLink.target(link) != nodeId
    ),
  }

  let mapNodes = (t, f) => {...t, nodes: t.nodes->Array.map(f)}

  let addLinks = (t, links) => {
    ...t,
    links: t.links->Array.concat(links),
  }
  let addLink = (t, link) => addLinks(t, [link])

  let mapLinks = (t, f) => {...t, links: t.links->Array.map(f)}
}

type t = {
  graph: Graph.t,
  selection: ReactD3Graph.Graph.Selection.t,
  nodeMap: Belt.Map.String.t<ModelNode.t>,
}

let init = {
  graph: Graph.empty,
  selection: ReactD3Graph.Graph.Selection.empty,
  nodeMap: Belt.Map.String.empty,
}

let data = t => {
  ReactD3Graph.Data.nodes: t.graph.nodes->Array.flatMap(node => [node.ModelNode.focus]),
  links: t.graph.links,
}

let nodeWithId = (t, nodeId) =>
  t.nodeMap->Belt.Map.String.get(nodeId->ReactD3Graph.Node.Id.toString)

let addNode = (t, node) => {
  ...t,
  nodeMap: t.nodeMap->Belt.Map.String.set(ModelNode.id(node)->ReactD3Graph.Node.Id.toString, node),
  graph: t.graph->Graph.addNode(node),
}

let removeNode = (t, nodeId) => {
  ...t,
  nodeMap: t.nodeMap->Belt.Map.String.remove(nodeId->ReactD3Graph.Node.Id.toString),
  graph: t.graph->Graph.removeNode(nodeId),
}

let addLink = (t, link) => {...t, graph: t.graph->Graph.addLink(link)}

let selection = t => t.selection

let setSelection = (t, selection) => {...t, selection: selection}
