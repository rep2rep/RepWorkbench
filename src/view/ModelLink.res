module Kind = {
  type t =
    | Hierarchy
    | Anchor
    | Generic

  let h_const = Hash.unique()
  let a_const = Hash.unique()
  let g_const = Hash.unique()
  let hash = t =>
    switch t {
    | Hierarchy => h_const
    | Anchor => a_const
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
      type t =
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

    module V3 = {
      type t = t =
        | Hierarchy
        | Anchor
        | Generic

      let v2_to_v3 = v2 =>
        switch v2 {
        | V2.Hierarchy => Hierarchy
        | V2.Anchor => Anchor
        | V2.Relation => Generic
        | V2.Overlap => Generic
        | V2.Disjoint => Generic
        | V2.Generic => Generic
        }

      let toJson = t =>
        switch t {
        | Hierarchy => "Hierarchy"
        | Anchor => "Anchor"
        | Generic => "Generic"
        }->String.toJson

      let fromJson = json =>
        json
        ->String.fromJson
        ->Or_error.flatMap(s =>
          switch s {
          | "Hierarchy" => Or_error.create(Hierarchy)
          | "Anchor" => Or_error.create(Anchor)
          | "Generic" => Or_error.create(Generic)
          | _ => json->V2.fromJson->Or_error.map(v2_to_v3)
          }
        )
    }
  }
}

module Payload = {
  type t = {kind: Kind.t, label: option<int>}

  let kind = t => t.kind
  let label = t => t.label
  let setLabel = (t, l) => {...t, label: l}

  let hash: t => Hash.t = Hash.record2(("kind", Kind.hash), ("label", Option.hash(_, Int.hash)))

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

    module V3 = {
      type t = t = {
        kind: Kind.Stable.V3.t,
        label: option<int>,
      }

      let v2_to_v3 = v2 => {
        kind: v2->Kind.Stable.V3.v2_to_v3,
        label: None,
      }

      let toJson = t =>
        Js.Dict.fromArray([
          ("version", 3->Int.toJson),
          ("kind", t.kind->Kind.Stable.V3.toJson),
          ("label", t.label->Option.toJson(Int.toJson)),
        ])->Js.Json.object_

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
          | Or_error.Ok(3) => {
              let kind = getValue("kind", Kind.Stable.V3.fromJson)
              let label = getValue("label", j => j->Option.fromJson(Int.fromJson))
              (kind, label)
              ->Or_error.both
              ->Or_error.map(((kind, label)) => {kind: kind, label: label})
            }
          | Or_error.Ok(v) =>
            Or_error.error_ss(["Unrecognised version of ModelLink: ", Int.toString(v)])
          | Or_error.Err(_) => json->V2.fromJson->Or_error.map(v2_to_v3)
          }
        })
    }
  }

  let create = s => {kind: s, label: None}
}

type t = ReactD3Graph.Link.t<Payload.t>

let data = t => [t]

module Config = {
  let hierarchy = order =>
    ReactD3Graph.Link.Config.create(
      ~offsetSource=(_, _, _) => {"dx": 0., "dy": 25.},
      ~offsetTarget=(_, _, _) => {"dx": 0., "dy": -25.},
      ~color=ReactD3Graph.Color.ofHexString("#000000"),
      ~strokeWidth=1.,
      ~labelProperty=_ =>
        order
        ->Option.map(Int.toString)
        ->Option.map(label => {
          <g>
            <circle r="8" cx="8" cy="8" fill="white" />
            <text textAnchor="middle" x="8" y="12" fontSize="0.8rem"> {React.string(label)} </text>
          </g>
        })
        ->Option.getWithDefault(React.null),
      (),
    )
  let anchor = order =>
    ReactD3Graph.Link.Config.create(
      ~offsetSource=(_, _, _) => {"dx": 0., "dy": 25.},
      ~offsetTarget=(_, _, _) => {"dx": 0., "dy": -25.},
      ~color=ReactD3Graph.Color.ofHexString("#000000"),
      ~strokeWidth=1.,
      ~markerStart="arrowheadCircle",
      ~labelProperty=_ =>
        order
        ->Option.map(Int.toString)
        ->Option.map(label => {
          <g>
            <circle r="8" cx="8" cy="8" fill="white" />
            <text textAnchor="middle" x="8" y="12" fontSize="0.8rem"> {React.string(label)} </text>
          </g>
        })
        ->Option.getWithDefault(React.null),
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
  let generic = ReactD3Graph.Link.Config.create(
    ~offsetSource=(source, target, _) => relOffset(source, target),
    ~offsetTarget=(source, target, _) => relOffset(target, source),
    ~color=ReactD3Graph.Color.ofHexString("#AA0000"),
    ~strokeWidth=5.,
    (),
  )
}

let create = (~linkId, ~source, ~target, kind, ~label) => {
  let config = switch kind {
  | Kind.Hierarchy => Config.hierarchy(label)
  | Kind.Anchor => Config.anchor(label)
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
let kind = t => t->payload->Option.map(p => p.Payload.kind)->Option.getWithDefault(Kind.Hierarchy)
let label = t => t->payload->Option.flatMap(p => p.Payload.label)
let updateConfig = (t, f) => t->ReactD3Graph.Link.updateConfig(f)
let updatePayload = (t, f) => {
  let t' = t->ReactD3Graph.Link.updatePayload(Option.map(_, f))
  let pl = t'->ReactD3Graph.Link.payload->Option.getExn
  let config = switch pl.Payload.kind {
  | Kind.Hierarchy => Config.hierarchy(pl.label)
  | Kind.Anchor => Config.anchor(pl.label)
  | Kind.Generic => Config.generic
  }
  t'->updateConfig(_ => config)
}

let hash = t =>
  Hash.combine([
    Gid.hash(id(t)),
    Gid.hash(source(t)),
    Gid.hash(target(t)),
    payload(t)->Option.hash(Payload.hash),
  ])

let duplicate = (t, idMap, nodes) => {
  let linkId = idMap->Gid.Map.get(id(t))->Option.getExn
  let sourceId = idMap->Gid.Map.get(source(t))->Option.getExn
  let source = nodes->Array.find(node => ModelNode.id(node) == sourceId)->Option.getExn
  let targetId = idMap->Gid.Map.get(target(t))->Option.getExn
  let target = nodes->Array.find(node => ModelNode.id(node) == targetId)->Option.getExn
  let kind = kind(t)
  let label = label(t)
  create(~linkId, ~source, ~target, kind, ~label)
}

// TODO: Check something!
let isValid = _ => Result.Ok()

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
          | Kind.Stable.V1.Hierarchy => Config.hierarchy(None)
          | Kind.Stable.V1.Anchor => Config.anchor(None)
          | Kind.Stable.V1.Relation => Config.generic
          }->Obj.magic
          ReactD3Graph.Link.create(~source, ~target, ~payload, ~config, ~id?, ())
        })
      })
  }

  module V2 = {
    type t = ReactD3Graph.Link.t<Payload.Stable.V2.t>

    let id = t => Obj.magic(t)["id"]->Gid.fromString
    let kind = t => Obj.magic(t)["payload"]->Option.getWithDefault(Kind.Stable.V2.Hierarchy)

    let v1_to_v2: V1.t => t = v1 => {
      // Horrible, horrible hack. Terrible, no good, very bad.
      let v1' = Obj.magic(v1)
      v1'["id"] = Gid.create()->Gid.toString
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
              | Kind.Stable.V2.Hierarchy => Config.hierarchy(None)
              | Kind.Stable.V2.Anchor => Config.anchor(None)
              | Kind.Stable.V2.Relation => Config.generic
              | Kind.Stable.V2.Overlap => Config.generic
              | Kind.Stable.V2.Disjoint => Config.generic
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

  module V3 = {
    type t = ReactD3Graph.Link.t<Payload.Stable.V3.t>

    let v2_to_v3: V2.t => t = v2 => {
      // Horrible, horrible hack. Terrible, no good, very bad.
      let v2' = Obj.magic(v2)
      v2'["payload"] = v2'["payload"]->Option.map(Payload.Stable.V3.v2_to_v3)
      Obj.magic(v2')
    }

    let toJson = t =>
      Js.Dict.fromList(list{
        ("version", 3->Int.toJson),
        ("source", source(t)->Gid.toJson),
        ("target", target(t)->Gid.toJson),
        ("id", id(t)->Gid.toJson),
        ("payload", payload(t)->Option.toJson(Payload.Stable.V3.toJson)),
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
        | Or_error.Ok(3) => {
            let source = getValue("source", Gid.fromJson)
            let target = getValue("target", Gid.fromJson)
            let id = getValue("id", Gid.fromJson)
            let payload = getValue("payload", json =>
              json->Option.fromJson(Payload.Stable.V3.fromJson)
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
              let payload =
                payload->Option.getWithDefault({kind: Kind.Stable.V3.Hierarchy, label: None})
              let config = switch payload.kind {
              | Kind.Stable.V3.Hierarchy => Config.hierarchy(payload.label)
              | Kind.Stable.V3.Anchor => Config.anchor(payload.label)
              | Kind.Stable.V3.Generic => Config.generic
              }->Obj.magic
              ReactD3Graph.Link.create(~source, ~target, ~payload, ~config, ~id, ())
            })
          }
        | Or_error.Ok(2) => json->V2.fromJson->Or_error.map(v2_to_v3)
        | Or_error.Ok(v) => Or_error.error_ss(["Unrecognised ModelLink version: ", Int.toString(v)])
        | Or_error.Err(_) => V1.fromJson(json)->Or_error.map(V2.v1_to_v2)->Or_error.map(v2_to_v3)
        }
      })
  }
}
