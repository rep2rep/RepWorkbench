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

let empty = ReactD3Graph.Graph.Selection.empty

let nodes = t => t.nodes
let links = t => t.links
