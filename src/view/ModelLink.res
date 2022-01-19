module Payload = {
  type t = string

  let create = s => s
}

type t = ReactD3Graph.Link.t<Payload.t>

module Kind = {
  type t =
    | Heirarchy
    | Anchor
    | Relation
}

module Config = {
  let heirarchy = ReactD3Graph.Link.Config.create(
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
  | Kind.Heirarchy => Config.heirarchy
  | Kind.Anchor => Config.anchor
  | Kind.Relation => Config.relation
  }
  ReactD3Graph.Link.create(
    ~source=source->ModelNode.id,
    ~target=target->ModelNode.id,
    ~payload=Payload.create("Hello"),
    ~config,
    (),
  )
}

let source = ReactD3Graph.Link.source
let target = ReactD3Graph.Link.target
