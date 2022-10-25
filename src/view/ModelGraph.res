type t = {
  nodes: array<ModelNode.t>,
  links: array<ModelLink.t>,
}

module Stable = {
  module V1 = {
    type t = {
      nodes: array<ModelNode.Stable.V1.t>,
      links: array<ModelLink.Stable.V1.t>,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("nodes", t.nodes->Array.toJson(ModelNode.Stable.V1.toJson)),
        ("links", t.links->Array.toJson(ModelLink.Stable.V1.toJson)),
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
        let nodes = getValue("nodes", j => j->Array.fromJson(ModelNode.Stable.V1.fromJson))
        let links = getValue("links", j => j->Array.fromJson(ModelLink.Stable.V1.fromJson))

        Or_error.both((nodes, links))->Or_error.map(((nodes, links)) => {
          nodes: nodes,
          links: links,
        })
      })
  }

  module V2 = {
    type t = {
      nodes: array<ModelNode.Stable.V2.t>,
      links: array<ModelLink.Stable.V1.t>,
    }

    let v1_to_v2 = v1 => {
      nodes: v1.V1.nodes->Array.map(ModelNode.Stable.V2.v1_to_v2),
      links: v1.V1.links,
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(2)),
        ("nodes", t.nodes->Array.toJson(ModelNode.Stable.V2.toJson)),
        ("links", t.links->Array.toJson(ModelLink.Stable.V1.toJson)),
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
        let version = getValue("version", Int.fromJson)
        if Or_error.isOk(version) {
          let v = Or_error.okExn(version)
          if v === 2 {
            let nodes = getValue("nodes", j => j->Array.fromJson(ModelNode.Stable.V2.fromJson))
            let links = getValue("links", j => j->Array.fromJson(ModelLink.Stable.V1.fromJson))

            Or_error.both((nodes, links))->Or_error.map(((nodes, links)) => {
              nodes: nodes,
              links: links,
            })
          } else {
            Or_error.error_ss(["Unknown ModelGraph version ", Int.toString(v)])
          }
        } else {
          V1.fromJson(json)->Or_error.map(v1_to_v2)
        }
      })
  }

  module V3 = {
    type t = {
      nodes: array<ModelNode.Stable.V2.t>,
      links: array<ModelLink.Stable.V2.t>,
    }

    let links = t => t.links

    let v2_to_v3 = v2 => {
      nodes: v2.V2.nodes,
      links: v2.V2.links->Array.map(ModelLink.Stable.V2.v1_to_v2),
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(3)),
        ("nodes", t.nodes->Array.toJson(ModelNode.Stable.V2.toJson)),
        ("links", t.links->Array.toJson(ModelLink.Stable.V2.toJson)),
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
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(3) => {
            let nodes = getValue("nodes", j => j->Array.fromJson(ModelNode.Stable.V2.fromJson))
            let links = getValue("links", j => j->Array.fromJson(ModelLink.Stable.V2.fromJson))

            Or_error.both((nodes, links))->Or_error.map(((nodes, links)) => {
              nodes: nodes,
              links: links,
            })
          }
        | Or_error.Ok(2) => V2.fromJson(json)->Or_error.map(v2_to_v3)
        | Or_error.Ok(v) => Or_error.error_ss(["Unknown ModelGraph version ", Int.toString(v)])
        | Or_error.Err(_) => V1.fromJson(json)->Or_error.map(V2.v1_to_v2)->Or_error.map(v2_to_v3)
        }
      })
  }

  module V4 = {
    type t = t = {
      nodes: array<ModelNode.Stable.V2.t>,
      links: array<ModelLink.Stable.V3.t>,
    }

    let v3_to_v4 = v3 => {
      nodes: v3.V3.nodes,
      links: v3.V3.links->Array.map(ModelLink.Stable.V3.v2_to_v3),
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", Int.toJson(4)),
        ("nodes", t.nodes->Array.toJson(ModelNode.Stable.V2.toJson)),
        ("links", t.links->Array.toJson(ModelLink.Stable.V3.toJson)),
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
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(4) => {
            let nodes = getValue("nodes", j => j->Array.fromJson(ModelNode.Stable.V2.fromJson))
            let links = getValue("links", j => j->Array.fromJson(ModelLink.Stable.V3.fromJson))

            Or_error.both((nodes, links))->Or_error.map(((nodes, links)) => {
              nodes: nodes,
              links: links,
            })
          }
        | Or_error.Ok(3) => V3.fromJson(json)->Or_error.map(v3_to_v4)
        | Or_error.Ok(2) => V2.fromJson(json)->Or_error.map(V3.v2_to_v3)->Or_error.map(v3_to_v4)
        | Or_error.Ok(v) => Or_error.error_ss(["Unknown ModelGraph version ", Int.toString(v)])
        | Or_error.Err(_) =>
          V1.fromJson(json)
          ->Or_error.map(V2.v1_to_v2)
          ->Or_error.map(V3.v2_to_v3)
          ->Or_error.map(v3_to_v4)
        }
      })
  }
}

let hash = Hash.record2(
  ("nodes", arr => arr->Array.hash(ModelNode.hash)),
  ("links", arr => arr->Array.hash(ModelLink.hash)),
)

let duplicate = (t, newIdMap) => {
  let nodes =
    t.nodes->Array.map(node =>
      node->ModelNode.dupWithNewId(newIdMap->Gid.Map.get(ModelNode.id(node))->Option.getExn)
    )
  let links = t.links->Array.map(ModelLink.duplicate(_, newIdMap, nodes))
  {
    nodes: nodes,
    links: links,
  }
}

let isValid = t => {
  let nodesValid = t.nodes->Array.map(ModelNode.isValid)->Result.allUnit(Array.concatMany)
  let linksValid = t.links->Array.map(ModelLink.isValid)->Result.allUnit(Array.concatMany)
  let linkEndsExist =
    t.links
    ->Array.map(link => {
      let sourceExists = t.nodes->Array.some(node => node->ModelNode.id == link->ModelLink.source)
      let targetExists = t.nodes->Array.some(node => node->ModelNode.id == link->ModelLink.target)
      [
        if sourceExists {
          Result.Ok()
        } else {
          Result.Error([
            "Link (" ++
            Gid.toString(ModelLink.id(link)) ++
            ") source does not exist: " ++
            Gid.toString(ModelLink.source(link)),
          ])
        },
        if targetExists {
          Result.Ok()
        } else {
          Result.Error([
            "Link (" ++
            Gid.toString(ModelLink.id(link)) ++
            ") target does not exist: " ++
            Gid.toString(ModelLink.target(link)),
          ])
        },
      ]->Result.allUnit(Array.concatMany)
    })
    ->Result.allUnit(Array.concatMany)
  [nodesValid, linksValid, linkEndsExist]->Result.allUnit(Array.concatMany)
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
let removeLink = (t, linkId) => {
  ...t,
  links: t.links->Array.filter(link => ModelLink.id(link) !== linkId),
}
let mapLinks = (t, f) => {...t, links: t.links->Array.map(f)}
