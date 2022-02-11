type t = {
  graph: ModelGraph.t,
  selection: ModelSelection.t,
}

module Stable = {
  module V1 = {
    type t = t = {
      graph: ModelGraph.Stable.V1.t,
      selection: ModelSelection.Stable.V1.t,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("graph", ModelGraph.Stable.V1.toJson(t.graph)),
        ("selection", ModelSelection.Stable.V1.toJson(t.selection)),
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
        let graph = getValue("graph", ModelGraph.Stable.V1.fromJson)
        let selection = getValue("selection", ModelSelection.Stable.V1.fromJson)

        Or_error.both((graph, selection))->Or_error.map(((graph, selection)) => {
          {
            graph: graph,
            selection: selection,
          }
        })
      })
  }
}

let duplicate = (t, newIdMap) => {
  graph: t.graph->ModelGraph.duplicate(newIdMap),
  selection: t.selection->ModelSelection.duplicate(newIdMap),
}

let empty = {
  graph: ModelGraph.empty,
  selection: ModelSelection.empty,
}

let graph = t => t.graph

let data = t => {
  ReactD3Graph.Data.nodes: t.graph->ModelGraph.nodes->Array.flatMap(ModelNode.data),
  links: t.graph->ModelGraph.links->Array.flatMap(ModelLink.data),
}

let nodeWithId = (t, nodeId) =>
  t.graph->ModelGraph.nodes->Array.find(node => ModelNode.id(node) == nodeId)

let addNode = (t, node) => {
  ...t,
  graph: t.graph->ModelGraph.addNode(node),
}

let updateNodes = (t, f) => {
  ...t,
  graph: t.graph->ModelGraph.mapNodes(f),
}

let removeNode = (t, nodeId) => {
  ...t,
  graph: t.graph->ModelGraph.removeNode(nodeId),
}

let addLink = (t, link) => {...t, graph: t.graph->ModelGraph.addLink(link)}

let removeLinks = (t, links) => {
  {...t, graph: t.graph->ModelGraph.removeLinks(links)}
}

let selection = t => t.selection

let setSelection = (t, selection) => {...t, selection: selection}
