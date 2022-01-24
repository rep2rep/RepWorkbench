type t = {
  graph: ModelGraph.t,
  selection: ModelSelection.t,
  nodeMap: Uuid.Map.t<ModelNode.t>,
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("graph", ModelGraph.toJson(t.graph)),
    ("selection", ModelSelection.toJson(t.selection)),
  })->Js.Json.object_

let fromJson = json =>
  json
  ->Js.Json.decodeObject
  ->Or_error.fromOption_s("Failed to decode state object JSON")
  ->Or_error.flatMap(dict => {
    let getValue = (key, reader) =>
      dict
      ->Js.Dict.get(key)
      ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
      ->Or_error.flatMap(reader)
    let graph = getValue("graph", ModelGraph.fromJson)
    let selection = getValue("selection", ModelSelection.fromJson)

    Or_error.both((graph, selection))->Or_error.map(((graph, selection)) => {
      let nodeMap =
        graph->ModelGraph.nodes->Array.map(node => (ModelNode.id(node), node))->Uuid.Map.fromArray
      {
        graph: graph,
        selection: selection,
        nodeMap: nodeMap,
      }
    })
  })

let empty = {
  graph: ModelGraph.empty,
  selection: ModelSelection.empty,
  nodeMap: Uuid.Map.empty(),
}

let data = t => {
  ReactD3Graph.Data.nodes: t.graph->ModelGraph.nodes->Array.flatMap(ModelNode.data),
  links: t.graph->ModelGraph.links->Array.flatMap(ModelLink.data),
}

let nodeWithId = (t, nodeId) => t.nodeMap->Uuid.Map.get(nodeId)

let addNode = (t, node) => {
  ...t,
  nodeMap: t.nodeMap->Uuid.Map.set(ModelNode.id(node), node),
  graph: t.graph->ModelGraph.addNode(node),
}

let updateNodes = (t, f) => {
  ...t,
  graph: t.graph->ModelGraph.mapNodes(f),
}

let removeNode = (t, nodeId) => {
  ...t,
  nodeMap: t.nodeMap->Uuid.Map.remove(nodeId),
  graph: t.graph->ModelGraph.removeNode(nodeId),
}

let addLink = (t, link) => {...t, graph: t.graph->ModelGraph.addLink(link)}

let selection = t => t.selection

let setSelection = (t, selection) => {...t, selection: selection}
