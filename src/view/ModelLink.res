module Kind = {
  type t =
    | Hierarchy
    | Anchor
    | Relation

  module Stable = {
    module V1 = {
      type t = t =
        | Hierarchy
        | Anchor
        | Relation

      let toJson = t =>
        switch t {
        | Hierarchy => "Hierarchy"
        | Anchor => "Anchor"
        | Relation => "Relation"
        }->String.toJson

      let fromJson = json =>
        json
        ->String.fromJson
        ->Or_error.flatMap(s =>
          switch s {
          | "Hierarchy" => Or_error.create(Hierarchy)
          | "Anchor" => Or_error.create(Anchor)
          | "Relation" => Or_error.create(Relation)
          | s => Or_error.error_ss(["Unknown relation value '", s, "'"])
          }
        )
    }
  }
}

module Payload = {
  type t = Kind.t

  module Stable = {
    module V1 = {
      type t = Kind.t

      let toJson = Kind.Stable.V1.toJson
      let fromJson = Kind.Stable.V1.fromJson
    }
  }

  let create = s => s
}

type t = ReactD3Graph.Link.t<Payload.t>

let data = t => [t]

module Config = {
  let hierarchy = ReactD3Graph.Link.Config.create(
    ~offsetSource=(_, _, _) => {"dx": 0., "dy": 25.},
    ~offsetTarget=(_, _, _) => {"dx": 0., "dy": -25.},
    ~color=ReactD3Graph.Color.ofHexString("#000000"),
    ~strokeWidth=1.,
    (),
  )
  let anchor = ReactD3Graph.Link.Config.create(
    ~offsetSource=(_, _, _) => {"dx": 0., "dy": 25.},
    ~offsetTarget=(_, _, _) => {"dx": 0., "dy": -25.},
    ~color=ReactD3Graph.Color.ofHexString("#000000"),
    ~strokeWidth=1.,
    ~markerStart="arrowheadCircle",
    (),
  )
  let relOffset = (source, target) =>
    (source, target)
    ->Option.both
    ->Option.map(((source, target)) => {
      let (x1, y1) = (source["x"], source["y"])
      let (x2, y2) = (target["x"], target["y"])
      let size = source->ReactD3Graph.Core.readKeyExn("size")
      let (dx0, dy0) = (x2 -. x1, y2 -. y1)
      let (dx, dy) = if Js.Math.abs_float(dx0 /. dy0) > size["width"] /. size["height"] {
        let dx = size["width"] /. 20. *. Js.Math.sign_float(dx0)
        let dy = dy0 *. dx /. dx0
        (dx, dy)
      } else {
        let dy = size["height"] /. 20. *. Js.Math.sign_float(dy0)
        let dx = dx0 *. dy /. dy0
        (dx, dy)
      }
      let payload: ModelNode.Payload.t = source->ReactD3Graph.Core.readKeyExn("payload")
      switch payload.kind {
      | ModelNode.Kind.Representation | Scheme | Token | Placeholder => {"dx": dx -. 1., "dy": dy}
      | Dimension =>
        if Js.Math.abs_float(dx -. size["width"] /. 20. *. Js.Math.sign_float(dx0)) < 0.0001 {
          // Need to do a "slant adjustment"
          let yoffset = size["height"] /. 10. -. dy +. size["height"] /. 20.
          let xoffset =
            (-8. +. 8. *. yoffset /. (size["height"] /. 10.)) *. -1. *. Js.Math.sign_float(dx)
          {"dx": dx +. xoffset -. 1., "dy": dy}
        } else {
          {"dx": dx -. 1., "dy": dy}
        }
      }
    })
    ->Option.getWithDefault({"dx": 0., "dy": 0.})
  let relation = ReactD3Graph.Link.Config.create(
    ~offsetSource=(source, target, _) => relOffset(source, target),
    ~offsetTarget=(source, target, _) => relOffset(target, source),
    ~color=ReactD3Graph.Color.ofHexString("#808080"),
    ~strokeWidth=4.,
    ~strokeDasharray=5.,
    (),
  )
}

let create = (~source, ~target, kind) => {
  let config = switch kind {
  | Kind.Hierarchy => Config.hierarchy
  | Kind.Anchor => Config.anchor
  | Kind.Relation => Config.relation
  }
  ReactD3Graph.Link.create(
    ~source=source->ModelNode.id->Gid.toString->ReactD3Graph.Node.Id.ofString,
    ~target=target->ModelNode.id->Gid.toString->ReactD3Graph.Node.Id.ofString,
    ~payload=Payload.create(kind),
    ~config,
    (),
  )
}

let source = t => t->ReactD3Graph.Link.source->ReactD3Graph.Node.Id.toString->Gid.fromString
let target = t => t->ReactD3Graph.Link.target->ReactD3Graph.Node.Id.toString->Gid.fromString
let id = t => t->ReactD3Graph.Link.id
let payload = t => t->ReactD3Graph.Link.payload
let kind = t => t->payload->Option.getWithDefault(Kind.Hierarchy)

module Stable = {
  module V1 = {
    type t = ReactD3Graph.Link.t<Payload.Stable.V1.t>

    let toJson = t =>
      Js.Dict.fromList(list{
        ("source", source(t)->Gid.toJson),
        ("target", target(t)->Gid.toJson),
        ("id", id(t)->Option.map(ReactD3Graph.Link.Id.toString)->Option.toJson(String.toJson)),
        ("payload", payload(t)->Option.toJson(Payload.Stable.V1.toJson)),
      })->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode ModelLink object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let source = getValue("source", Gid.fromJson)
        let target = getValue("target", Gid.fromJson)
        let id = getValue("id", json =>
          json
          ->Option.fromJson(String.fromJson)
          ->Or_error.map(s => s->Option.map(ReactD3Graph.Link.Id.ofString))
        )
        let payload = getValue("payload", json => json->Option.fromJson(Payload.Stable.V1.fromJson))

        Or_error.both4((source, target, id, payload))->Or_error.map(((
          source,
          target,
          id,
          payload,
        )) => {
          let source = source->Gid.toString->ReactD3Graph.Node.Id.ofString
          let target = target->Gid.toString->ReactD3Graph.Node.Id.ofString
          let payload = payload->Option.getWithDefault(Kind.Hierarchy)
          let config = switch payload {
          | Kind.Hierarchy => Config.hierarchy
          | Kind.Anchor => Config.anchor
          | Kind.Relation => Config.relation
          }
          ReactD3Graph.Link.create(~source, ~target, ~payload, ~config, ~id?, ())
        })
      })
  }
}
