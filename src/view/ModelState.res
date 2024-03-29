type t = {
  graph: ModelGraph.t,
  selection: ModelSelection.t,
}

module Stable = {
  module V1 = {
    type t = {
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

  module V2 = {
    type t = {
      graph: ModelGraph.Stable.V1.t,
      selection: ModelSelection.Stable.V1.t,
    }

    let v1_to_v2 = t => {
      graph: t.V1.graph,
      selection: t.V1.selection,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(2)),
        ("graph", ModelGraph.Stable.V1.toJson(t.graph)),
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
        let version = getValue("version", Int.fromJson)
        if !Or_error.isOk(version) {
          Js.Console.log("Upgrading ModelState from V1 to V2...")
          V1.fromJson(json)->Or_error.map(v1_to_v2)
        } else if Or_error.okExn(version) != 2 {
          Or_error.error_ss([
            "Attempting to read unsupported ModelState version ",
            Int.toString(Or_error.okExn(version)),
            "!",
          ])
        } else {
          let graph = getValue("graph", ModelGraph.Stable.V1.fromJson)

          graph->Or_error.map(graph => {
            {
              graph: graph,
              selection: ModelSelection.empty,
            }
          })
        }
      })
  }

  module V3 = {
    type t = {
      graph: ModelGraph.Stable.V2.t,
      selection: ModelSelection.Stable.V1.t,
    }

    let v2_to_v3 = t => {
      graph: t.V2.graph->ModelGraph.Stable.V2.v1_to_v2,
      selection: t.V2.selection,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(3)),
        ("graph", ModelGraph.Stable.V2.toJson(t.graph)),
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
        let version = getValue("version", Int.fromJson)
        if !Or_error.isOk(version) {
          Js.Console.log("Upgrading ModelState from V1 to V3...")
          V1.fromJson(json)->Or_error.map(V2.v1_to_v2)->Or_error.map(v2_to_v3)
        } else if Or_error.okExn(version) == 2 {
          Js.Console.log("Upgrading ModelState from V2 to V3...")
          V2.fromJson(json)->Or_error.map(v2_to_v3)
        } else if Or_error.okExn(version) == 3 {
          let graph = getValue("graph", ModelGraph.Stable.V2.fromJson)

          graph->Or_error.map(graph => {
            {
              graph: graph,
              selection: ModelSelection.empty,
            }
          })
        } else {
          Or_error.error_ss([
            "Attempting to read unsupported ModelState version ",
            Int.toString(Or_error.okExn(version)),
            "!",
          ])
        }
      })
  }

  module V4 = {
    type t = {
      graph: ModelGraph.Stable.V3.t,
      selection: ModelSelection.Stable.V1.t,
    }
    let graph = t => t.graph

    let v3_to_v4 = t => {
      graph: t.V3.graph->ModelGraph.Stable.V3.v2_to_v3,
      selection: t.V3.selection,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(4)),
        ("graph", ModelGraph.Stable.V3.toJson(t.graph)),
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
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(4) => {
            let graph = getValue("graph", ModelGraph.Stable.V3.fromJson)
            graph->Or_error.map(graph => {
              {
                graph: graph,
                selection: ModelSelection.empty,
              }
            })
          }
        | Or_error.Ok(3) => {
            Js.Console.log("Upgrading ModelState from V3 to V4...")
            json->V3.fromJson->Or_error.map(v3_to_v4)
          }
        | Or_error.Ok(2) => {
            Js.Console.log("Upgrading ModelState from V2 to V4...")
            json->V2.fromJson->Or_error.map(V3.v2_to_v3)->Or_error.map(v3_to_v4)
          }
        | Or_error.Ok(v) =>
          Or_error.error_ss([
            "Attempting to read unsupported ModelState version ",
            Int.toString(v),
            "!",
          ])
        | Or_error.Err(_) => {
            Js.Console.log("Upgrading ModelState from V1 to V3...")
            json
            ->V1.fromJson
            ->Or_error.map(V2.v1_to_v2)
            ->Or_error.map(V3.v2_to_v3)
            ->Or_error.map(v3_to_v4)
          }
        }
      })
  }

  module V5 = {
    type t = t = {
      graph: ModelGraph.Stable.V4.t,
      selection: ModelSelection.Stable.V1.t,
    }

    let v4_to_v5 = t => {
      graph: t.V4.graph->ModelGraph.Stable.V4.v3_to_v4,
      selection: t.V4.selection,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(5)),
        ("graph", ModelGraph.Stable.V4.toJson(t.graph)),
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
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(5) => {
            let graph = getValue("graph", ModelGraph.Stable.V4.fromJson)
            graph->Or_error.map(graph => {
              {
                graph: graph,
                selection: ModelSelection.empty,
              }
            })
          }
        | Or_error.Ok(4) => {
            Js.Console.log("Upgrading ModelState from V4 to V5...")
            json->V4.fromJson->Or_error.map(v4_to_v5)
          }
        | Or_error.Ok(3) => {
            Js.Console.log("Upgrading ModelState from V3 to V5...")
            json->V3.fromJson->Or_error.map(V4.v3_to_v4)->Or_error.map(v4_to_v5)
          }
        | Or_error.Ok(2) => {
            Js.Console.log("Upgrading ModelState from V2 to V5...")
            json
            ->V2.fromJson
            ->Or_error.map(V3.v2_to_v3)
            ->Or_error.map(V4.v3_to_v4)
            ->Or_error.map(v4_to_v5)
          }
        | Or_error.Ok(v) =>
          Or_error.error_ss([
            "Attempting to read unsupported ModelState version ",
            Int.toString(v),
            "!",
          ])
        | Or_error.Err(_) => {
            Js.Console.log("Upgrading ModelState from V1 to V5...")
            json
            ->V1.fromJson
            ->Or_error.map(V2.v1_to_v2)
            ->Or_error.map(V3.v2_to_v3)
            ->Or_error.map(V4.v3_to_v4)
            ->Or_error.map(v4_to_v5)
          }
        }
      })
  }
}

let hash = Hash.record1("graph", ModelGraph.hash)

let isValid = t => {
  let graphValid = t.graph->ModelGraph.isValid
  let selectionValid = t.selection->ModelSelection.isValid
  let allSelectionNodesExist =
    t.selection
    ->ModelSelection.nodes
    ->Array.map(id => {
      let nodeExists = t.graph->ModelGraph.nodes->Array.some(node => ModelNode.id(node) == id)
      if nodeExists {
        Result.Ok()
      } else {
        Result.Error(["Selected node does not exist! " ++ Gid.toString(id)])
      }
    })
    ->Result.allUnit(Array.concatMany)
  let allSelectionLinksExist =
    t.selection
    ->ModelSelection.links
    ->Array.map(id => {
      let linkExists = t.graph->ModelGraph.links->Array.some(link => ModelLink.id(link) == id)
      if linkExists {
        Result.Ok()
      } else {
        Result.Error(["Selected link does not exist! " ++ Gid.toString(id)])
      }
    })
    ->Result.allUnit(Array.concatMany)
  [graphValid, selectionValid, allSelectionNodesExist, allSelectionLinksExist]->Result.allUnit(
    Array.concatMany,
  )
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

let linkWithId = (t, linkId) =>
  t.graph->ModelGraph.links->Array.find(link => ModelLink.id(link) == linkId)

let addNode = (t, node) => {
  ...t,
  graph: t.graph->ModelGraph.addNode(node),
}

let addNodes = (t, nodes) => {
  ...t,
  graph: t.graph->ModelGraph.addNodes(nodes),
}

let updateNodes = (t, f) => {
  ...t,
  graph: t.graph->ModelGraph.mapNodes(f),
}

let updateLinks = (t, f) => {
  ...t,
  graph: t.graph->ModelGraph.mapLinks(f),
}

let moveNode = (t, id, ~x, ~y) =>
  t->updateNodes(node =>
    if ModelNode.id(node) == id {
      node->ModelNode.setPosition(~x, ~y)
    } else {
      node
    }
  )

let removeNode = (t, nodeId) => {
  ...t,
  graph: t.graph->ModelGraph.removeNode(nodeId),
}

let addLink = (t, link) => {...t, graph: t.graph->ModelGraph.addLink(link)}

let addLinks = (t, links) => {...t, graph: t.graph->ModelGraph.addLinks(links)}

let removeLink = (t, link) => {
  {...t, graph: t.graph->ModelGraph.removeLink(link)}
}

let selection = t => t.selection

let setSelection = (t, selection) => {...t, selection: selection}

let duplicateNodes = (t, idMap) => {
  let newNodes =
    idMap
    ->Gid.Map.toArray
    ->Array.mapPartial(((oldId, newId)) =>
      t
      ->nodeWithId(oldId)
      ->Option.map(node => {
        let (x, y) = ModelNode.position(node)
        let (x, y) = (x +. 10., y +. 10.)
        let newNode = ModelNode.dupWithNewId(node, newId)->ModelNode.setPosition(~x, ~y)
        (newId, newNode)
      })
    )
    ->Gid.Map.fromArray
  let newLinks =
    t
    ->graph
    ->ModelGraph.links
    ->Array.mapPartial(link => {
      let oldId = ModelLink.id(link)
      let source = ModelLink.source(link)
      let target = ModelLink.target(link)
      switch (idMap->Gid.Map.get(source), idMap->Gid.Map.get(target)) {
      | (Some(newSource), Some(newTarget)) =>
        ModelLink.create(
          ~linkId=idMap->Gid.Map.get(oldId)->Option.getExn,
          ~source=newNodes->Gid.Map.get(newSource)->Option.getExn,
          ~target=newNodes->Gid.Map.get(newTarget)->Option.getExn,
          ModelLink.kind(link),
          ~label={ModelLink.label(link)},
        )->Some
      | _ => None
      }
    })
  t->addNodes(newNodes->Gid.Map.values)->addLinks(newLinks)
}

let incidentLinks = (t, ~nodeId) => {
  let (incoming, outgoing) =
    t
    ->graph
    ->ModelGraph.links
    ->Array.mapPartial(link => {
      let source = ModelLink.source(link)
      let target = ModelLink.target(link)
      let id = ModelLink.id(link)
      if source === nodeId {
        Some((false, id))
      } else if target === nodeId {
        Some((true, id))
      } else {
        None
      }
    })
    ->Array.partition(((isIncoming, _)) => isIncoming)
  let incoming = incoming->Array.map(((_, id)) => id)
  let outgoing = outgoing->Array.map(((_, id)) => id)
  {"incoming": Gid.Set.fromArray(incoming), "outgoing": Gid.Set.fromArray(outgoing)}
}

let linksConnectingNodes = (t, nodes) => {
  let links = nodes->Array.map(nodeId => t->incidentLinks(~nodeId))
  let (allIncoming, allOutgoing) =
    links->Array.reduce((Gid.Set.empty, Gid.Set.empty), ((incoming, outgoing), newEdges) => (
      incoming->Gid.Set.union(newEdges["incoming"]),
      outgoing->Gid.Set.union(newEdges["outgoing"]),
    ))
  Gid.Set.intersect(allIncoming, allOutgoing)->Gid.Set.toArray
}
