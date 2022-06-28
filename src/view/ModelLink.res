module Kind = {
  type t =
    | Hierarchy
    | Anchor
    | Relation
    | Overlap
    | Disjoint
    | Generic

  let h_const = Hash.unique()
  let a_const = Hash.unique()
  let r_const = Hash.unique()
  let o_const = Hash.unique()
  let d_const = Hash.unique()
  let g_const = Hash.unique()
  let hash = t =>
    switch t {
    | Hierarchy => h_const
    | Anchor => a_const
    | Relation => r_const
    | Overlap => o_const
    | Disjoint => d_const
    | Generic => g_const
    }

  module Stable = {
    module V1 = {
      type t =
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

    module V2 = {
      type t = t =
        | Hierarchy
        | Anchor
        | Relation
        | Overlap
        | Disjoint
        | Generic

      let toJson = t =>
        switch t {
        | Hierarchy => "Hierarchy"
        | Anchor => "Anchor"
        | Relation => "Relation"
        | Overlap => "Overlap"
        | Disjoint => "Disjoint"
        | Generic => "Generic"
        }->String.toJson

      let fromJson = json =>
        json
        ->String.fromJson
        ->Or_error.flatMap(s =>
          switch s {
          | "Hierarchy" => Or_error.create(Hierarchy)
          | "Anchor" => Or_error.create(Anchor)
          | "Relation" => Or_error.create(Relation)
          | "Overlap" => Or_error.create(Overlap)
          | "Disjoint" => Or_error.create(Disjoint)
          | "Generic" => Or_error.create(Generic)
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
      type t = Kind.Stable.V1.t

      let toJson = Kind.Stable.V1.toJson
      let fromJson = Kind.Stable.V1.fromJson
    }

    module V2 = {
      type t = Kind.Stable.V2.t

      let toJson = Kind.Stable.V2.toJson
      let fromJson = Kind.Stable.V2.fromJson
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
  let overlapOffset = (source, target) =>
    (source, target)
    ->Option.both
    ->Option.map(((source, target)) => {
      let x1 = source["x"]
      let x2 = target["x"]
      let width = (source->ReactD3Graph.Core.readKeyExn("size"))["width"]
      let payload: ModelNode.Payload.t = source->ReactD3Graph.Core.readKeyExn("payload")
      let width = switch payload.kind {
      | Representation | Scheme | Token | Placeholder => width
      | Dimension => width -. 80.
      }
      let dx = if x1 < x2 {
        // Come out of right hand edge
        width /. 20. -. 4.
      } else {
        // Come out of left hand edge
        width /. -20. +. 1.
      }
      {"dx": dx, "dy": 0.}
    })
    ->Option.getWithDefault({"dx": 0., "dy": 0.})
  let overlap = ReactD3Graph.Link.Config.create(
    ~offsetSource=(source, target, _) => overlapOffset(source, target),
    ~offsetTarget=(source, target, _) => overlapOffset(target, source),
    ~color=ReactD3Graph.Color.ofHexString("#000000"),
    ~strokeWidth=2.,
    ~strokeDasharray=5.,
    ~markerStart="arrowheadCircleSmall",
    ~markerEnd="arrowheadCircleSmall",
    (),
  )
  let disjoint = ReactD3Graph.Link.Config.create(
    ~offsetSource=(source, target, _) => overlapOffset(source, target),
    ~offsetTarget=(source, target, _) => overlapOffset(target, source),
    ~color=ReactD3Graph.Color.ofHexString("#000000"),
    ~strokeWidth=2.,
    ~strokeDasharray=5.,
    ~markerStart="arrowheadDiamond",
    ~markerEnd="arrowheadDiamond",
    (),
  )
  let generic = ReactD3Graph.Link.Config.create(
    ~offsetSource=(source, target, _) => relOffset(source, target),
    ~offsetTarget=(source, target, _) => relOffset(target, source),
    ~color=ReactD3Graph.Color.ofHexString("#AA0000"),
    ~strokeWidth=5.,
    (),
  )
}

let create = (~linkId, ~source, ~target, kind) => {
  let config = switch kind {
  | Kind.Hierarchy => Config.hierarchy
  | Kind.Anchor => Config.anchor
  | Kind.Relation => Config.relation
  | Kind.Overlap => Config.overlap
  | Kind.Disjoint => Config.disjoint
  | Kind.Generic => Config.generic
  }
  ReactD3Graph.Link.create(
    ~id=linkId->Gid.toString->ReactD3Graph.Link.Id.ofString,
    ~source=source->ModelNode.id->Gid.toString->ReactD3Graph.Node.Id.ofString,
    ~target=target->ModelNode.id->Gid.toString->ReactD3Graph.Node.Id.ofString,
    ~payload=Payload.create(kind),
    ~config,
    (),
  )
}

let source = t => t->ReactD3Graph.Link.source->ReactD3Graph.Node.Id.toString->Gid.fromString
let target = t => t->ReactD3Graph.Link.target->ReactD3Graph.Node.Id.toString->Gid.fromString
let id = t =>
  t
  ->ReactD3Graph.Link.id
  ->Option.map(id => id->ReactD3Graph.Link.Id.toString->Gid.fromString)
  ->Option.getExn
let payload = t => t->ReactD3Graph.Link.payload
let kind = t => t->payload->Option.getWithDefault(Kind.Hierarchy)

let hash = t =>
  Hash.combine([Gid.hash(id(t)), Gid.hash(source(t)), Gid.hash(target(t)), Kind.hash(kind(t))])

module Stable = {
  module V1 = {
    type t = ReactD3Graph.Link.t<Payload.Stable.V1.t>

    let toJson = t =>
      Js.Dict.fromList(list{
        ("source", source(t)->Gid.toJson),
        ("target", target(t)->Gid.toJson),
        (
          "id",
          t
          ->ReactD3Graph.Link.id
          ->Option.map(ReactD3Graph.Link.Id.toString)
          ->Option.toJson(String.toJson),
        ),
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
          let payload = payload->Option.getWithDefault(Kind.Stable.V1.Hierarchy)
          let config = switch payload {
          | Kind.Stable.V1.Hierarchy => Config.hierarchy
          | Kind.Stable.V1.Anchor => Config.anchor
          | Kind.Stable.V1.Relation => Config.relation
          }->Obj.magic
          ReactD3Graph.Link.create(~source, ~target, ~payload, ~config, ~id?, ())
        })
      })
  }

  module V2 = {
    type t = ReactD3Graph.Link.t<Payload.Stable.V2.t>

    let v1_to_v2: V1.t => t = v1 => {
      // Horrible, horrible hack. Terrible, no good, very bad.
      let v1' = Obj.magic(v1)
      v1'["id"] = Gid.create()
      Obj.magic(v1')
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", 2->Int.toJson),
        ("source", source(t)->Gid.toJson),
        ("target", target(t)->Gid.toJson),
        ("id", id(t)->Gid.toJson),
        ("payload", payload(t)->Option.toJson(Payload.Stable.V2.toJson)),
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
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(2) => {
            let source = getValue("source", Gid.fromJson)
            let target = getValue("target", Gid.fromJson)
            let id = getValue("id", Gid.fromJson)
            let payload = getValue("payload", json =>
              json->Option.fromJson(Payload.Stable.V2.fromJson)
            )

            Or_error.both4((source, target, id, payload))->Or_error.map(((
              source,
              target,
              id,
              payload,
            )) => {
              let id = id->Gid.toString->ReactD3Graph.Link.Id.ofString
              let source = source->Gid.toString->ReactD3Graph.Node.Id.ofString
              let target = target->Gid.toString->ReactD3Graph.Node.Id.ofString
              let payload = payload->Option.getWithDefault(Kind.Stable.V2.Hierarchy)
              let config = switch payload {
              | Kind.Stable.V2.Hierarchy => Config.hierarchy
              | Kind.Stable.V2.Anchor => Config.anchor
              | Kind.Stable.V2.Relation => Config.relation
              | Kind.Stable.V2.Overlap => Config.overlap
              | Kind.Stable.V2.Disjoint => Config.disjoint
              | Kind.Stable.V2.Generic => Config.generic
              }->Obj.magic
              ReactD3Graph.Link.create(~source, ~target, ~payload, ~config, ~id, ())
            })
          }
        | Or_error.Ok(v) => Or_error.error_ss(["Unrecognised ModelLink version: ", Int.toString(v)])
        | Or_error.Err(_) => V1.fromJson(json)->Or_error.map(v1_to_v2)
        }
      })
  }
}
