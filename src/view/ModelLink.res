module Kind = {
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

module Payload = {
  type t = Kind.t

  let toJson = Kind.toJson
  let fromJson = Kind.fromJson

  let create = s => s
}

type t = ReactD3Graph.Link.t<Payload.t>

let data = t => [t]

module Config = {
  let hierarchy = ReactD3Graph.Link.Config.create(
    ~offsetSource={"dx": 0., "dy": 25.},
    ~offsetTarget={"dx": 0., "dy": -25.},
    ~color=ReactD3Graph.Color.ofHexString("#000000"),
    ~strokeWidth=1.,
    (),
  )
  let anchor = ReactD3Graph.Link.Config.create()
  let relation = ReactD3Graph.Link.Config.create()
}

let create = (~source, ~target, kind) => {
  let config = switch kind {
  | Kind.Hierarchy => Config.hierarchy
  | Kind.Anchor => Config.anchor
  | Kind.Relation => Config.relation
  }
  ReactD3Graph.Link.create(
    ~source=source->ModelNode.id->Uuid.toString->ReactD3Graph.Node.Id.ofString,
    ~target=target->ModelNode.id->Uuid.toString->ReactD3Graph.Node.Id.ofString,
    ~payload=Payload.create(kind),
    ~config,
    (),
  )
}

let source = t => t->ReactD3Graph.Link.source->ReactD3Graph.Node.Id.toString->Uuid.fromString
let target = t => t->ReactD3Graph.Link.target->ReactD3Graph.Node.Id.toString->Uuid.fromString
let id = t => t->ReactD3Graph.Link.id
let payload = t => t->ReactD3Graph.Link.payload

let toJson = t =>
  Js.Dict.fromList(list{
    ("source", source(t)->Uuid.toJson),
    ("target", target(t)->Uuid.toJson),
    ("id", id(t)->Option.map(ReactD3Graph.Link.Id.toString)->Option.toJson(String.toJson)),
    ("payload", payload(t)->Option.toJson(Payload.toJson)),
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
    let source = getValue("source", Uuid.fromJson)
    let target = getValue("target", Uuid.fromJson)
    let id = getValue("id", json =>
      json
      ->Option.fromJson(String.fromJson)
      ->Or_error.map(s => s->Option.map(ReactD3Graph.Link.Id.ofString))
    )
    let payload = getValue("payload", json => json->Option.fromJson(Payload.fromJson))

    Or_error.both4((source, target, id, payload))->Or_error.map(((source, target, id, payload)) => {
      let source = source->Uuid.toString->ReactD3Graph.Node.Id.ofString
      let target = target->Uuid.toString->ReactD3Graph.Node.Id.ofString
      let payload = payload->Option.getWithDefault(Kind.Hierarchy)
      let config = switch payload {
      | Kind.Hierarchy => Config.hierarchy
      | Kind.Anchor => Config.anchor
      | Kind.Relation => Config.relation
      }
      ReactD3Graph.Link.create(~source, ~target, ~payload, ~config, ~id?, ())
    })
  })
