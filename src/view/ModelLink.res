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
  let heirarchy = ReactD3Graph.Link.Config.create()
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
    ~source=source.ModelNode.child_connector->ReactD3Graph.Node.id,
    ~target=target.ModelNode.parent_connector->ReactD3Graph.Node.id,
    ~payload=Payload.create("Hello"),
    ~config,
    (),
  )
}
