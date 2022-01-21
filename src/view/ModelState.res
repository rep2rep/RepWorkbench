module Selection = {
  type t = ReactD3Graph.Graph.Selection.t = {
    nodes: array<ReactD3Graph.Node.Id.t>,
    links: array<ReactD3Graph.Link.Id.t>,
  }

  let toJson = t =>
    Js.Dict.fromList(list{
      ("nodes", t.nodes->Array.map(ReactD3Graph.Node.Id.toString)->Array.toJson(String.toJson)),
      ("links", t.links->Array.map(ReactD3Graph.Link.Id.toString)->Array.toJson(String.toJson)),
    })->Js.Json.object_

  let fromJson = json =>
    json
    ->Js.Json.decodeObject
    ->Or_error.fromOption_s("Failed to decode selection object JSON")
    ->Or_error.flatMap(dict => {
      let getValue = (key, reader) =>
        dict
        ->Js.Dict.get(key)
        ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
        ->Or_error.flatMap(reader)
      let nodes = getValue("nodes", json =>
        json
        ->Array.fromJson(String.fromJson)
        ->Or_error.map(arr => arr->Array.map(ReactD3Graph.Node.Id.ofString))
      )
      let links = getValue("links", json =>
        json
        ->Array.fromJson(String.fromJson)
        ->Or_error.map(arr => arr->Array.map(ReactD3Graph.Link.Id.ofString))
      )

      Or_error.both((nodes, links))->Or_error.map(((nodes, links)) => {
        nodes: nodes,
        links: links,
      })
    })
}

module T = {
  let key = "RepNotation:ModelState"

  type t = {
    graph: ModelGraph.t,
    selection: ReactD3Graph.Graph.Selection.t,
    nodeMap: Belt.Map.String.t<ModelNode.t>,
  }

  let toJson = t =>
    Js.Dict.fromList(list{
      ("graph", ModelGraph.toJson(t.graph)),
      ("selection", Selection.toJson(t.selection)),
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
      let selection = getValue("selection", Selection.fromJson)

      Or_error.both((graph, selection))->Or_error.map(((graph, selection)) => {
        let nodeMap =
          graph
          ->ModelGraph.nodes
          ->Array.map(node => (ModelNode.id(node)->ReactD3Graph.Node.Id.toString, node))
          ->Belt.Map.String.fromArray
        {
          graph: graph,
          selection: selection,
          nodeMap: nodeMap,
        }
      })
    })
}

include T

module Storage = LocalStorage.MakeJsonable(T)

let load = () => Storage.get()->Or_error.toOption
let save = Storage.set

let init = {
  graph: ModelGraph.empty,
  selection: ReactD3Graph.Graph.Selection.empty,
  nodeMap: Belt.Map.String.empty,
}

let data = t => {
  ReactD3Graph.Data.nodes: t.graph->ModelGraph.nodes->Array.flatMap(ModelNode.data),
  links: t.graph->ModelGraph.links->Array.flatMap(ModelLink.data),
}

let nodeWithId = (t, nodeId) =>
  t.nodeMap->Belt.Map.String.get(nodeId->ReactD3Graph.Node.Id.toString)

let addNode = (t, node) => {
  ...t,
  nodeMap: t.nodeMap->Belt.Map.String.set(ModelNode.id(node)->ReactD3Graph.Node.Id.toString, node),
  graph: t.graph->ModelGraph.addNode(node),
}

let updateNodes = (t, f) => {
  ...t,
  graph: t.graph->ModelGraph.mapNodes(f),
}

let removeNode = (t, nodeId) => {
  ...t,
  nodeMap: t.nodeMap->Belt.Map.String.remove(nodeId->ReactD3Graph.Node.Id.toString),
  graph: t.graph->ModelGraph.removeNode(nodeId),
}

let addLink = (t, link) => {...t, graph: t.graph->ModelGraph.addLink(link)}

let selection = t => t.selection

let setSelection = (t, selection) => {...t, selection: selection}
