type t = {
  nodes: array<ModelNode.t>,
  links: array<ModelLink.t>,
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("nodes", t.nodes->Array.toJson(ModelNode.toJson)),
    ("links", t.links->Array.toJson(ModelLink.toJson)),
  })->Js.Json.object_

let fromJson = json =>
  json
  ->Js.Json.decodeObject
  ->Or_error.fromOption_s("Failed to decode Graph object JSON")
  ->Or_error.flatMap(dict => {
    let getValue = (key, reader) =>
      dict
      ->Js.Dict.get(key)
      ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
      ->Or_error.flatMap(reader)
    let nodes = getValue("nodes", j => j->Array.fromJson(ModelNode.fromJson))
    let links = getValue("links", j => j->Array.fromJson(ModelLink.fromJson))

    Or_error.both((nodes, links))->Or_error.map(((nodes, links)) => {
      nodes: nodes,
      links: links,
    })
  })

let duplicate = (t, newIdMap) => {
  let nodes =
    t.nodes->Array.map(node =>
      node->ModelNode.dupWithNewId(newIdMap->Uuid.Map.get(ModelNode.id(node))->Option.getExn)
    )
  {
    nodes: nodes,
    links: t.links->Array.map(link => {
      let sourceId = newIdMap->Uuid.Map.get(ModelLink.source(link))->Option.getExn
      let source = nodes->Array.find(node => ModelNode.id(node) == sourceId)->Option.getExn
      let targetId = newIdMap->Uuid.Map.get(ModelLink.target(link))->Option.getExn
      let target = nodes->Array.find(node => ModelNode.id(node) == targetId)->Option.getExn
      let kind = ModelLink.kind(link)
      ModelLink.create(~source, ~target, kind)
    }),
  }
}

let empty = {
  nodes: [],
  links: [],
}

let nodes = t => t.nodes
let links = t => t.links

let addNodes = (t, nodes) => {
  ...t,
  nodes: Array.concat(t.nodes, nodes),
}
let addNode = (t, node) => addNodes(t, [node])

let removeNode = (t, nodeId) => {
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
let removeLinks = (t, toRemove) => {
  ...t,
  links: t.links->Array.filter(link => !(toRemove->Array.includes(link))),
}
