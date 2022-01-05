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

  let mapNodes = (t, f) => {...t, nodes: t.nodes->Array.map(f)}

  let addLinks = (t, links) => {
    ...t,
    links: t.links->Array.concat(links),
  }
  let addLink = (t, link) => addLinks(t, [link])

  let mapLinks = (t, f) => {...t, links: t.links->Array.map(f)}
}

module Selection = {
  type t = array<ReactD3Graph.Node.Id.t>

  let empty = []

  let addNodes = (t, nodeIds) => t->Array.concat(nodeIds)
  let addNode = (t, nodeId) => addNodes(t, [nodeId])
  let removeNodes = (t, nodeIds) => t->Array.filter(id => !Array.includes(nodeIds, id))
  let removeNode = (t, nodeId) => removeNodes(t, [nodeId])
  let contains = (t, nodeId) => t->Array.includes(nodeId)
}

type t = {
  graph: Graph.t,
  selection: Selection.t,
  nodeMap: Belt.Map.String.t<ModelNode.t>,
}

let init = {
  graph: Graph.empty,
  selection: Selection.empty,
  nodeMap: Belt.Map.String.empty,
}

let data = t => {
  ReactD3Graph.Data.nodes: t.graph.nodes->Array.flatMap(node => [
    node.ModelNode.focus,
    node.ModelNode.parent_connector,
    node.ModelNode.child_connector,
  ]),
  links: t.graph.links,
}

let removeSuffix = (id, suffix) =>
  id->String.replaceByRe(Js.Re.fromString("/" ++ suffix ++ "$/"), "")

let nodeWithId = (t, nodeId) =>
  t.nodeMap->Belt.Map.String.get(
    nodeId->ReactD3Graph.Node.Id.toString->removeSuffix("_parents")->removeSuffix("_children"),
  )

let addNode = (t, node) => {
  ...t,
  nodeMap: t.nodeMap->Belt.Map.String.set(ModelNode.id(node)->ReactD3Graph.Node.Id.toString, node),
  graph: t.graph->Graph.addNode(node),
}

let addLink = (t, link) => {...t, graph: t.graph->Graph.addLink(link)}

let addToSelection = (t, nodeId) => {
  ...t,
  graph: t.graph->Graph.mapNodes(node =>
    if ModelNode.id(node) == nodeId {
      node->ModelNode.setSelected(true)
    } else {
      node
    }
  ),
  selection: t.selection->Selection.addNode(nodeId),
}

let removeFromSelection = (t, nodeId) => {
  ...t,
  graph: t.graph->Graph.mapNodes(node =>
    if ModelNode.id(node) == nodeId {
      node->ModelNode.setSelected(false)
    } else {
      node
    }
  ),
  selection: t.selection->Selection.removeNode(nodeId),
}

let clearSelection = t => {
  ...t,
  graph: t.graph->Graph.mapNodes(node => node->ModelNode.setSelected(false)),
  selection: Selection.empty,
}

let selectedNodeIds = t => t.selection

let nodeIsSelected = (t, nodeId) => t.selection->Selection.contains(nodeId)
