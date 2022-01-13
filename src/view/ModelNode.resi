module Payload: {
  type t = {
    name: string,
    reference: string,
    selected: bool,
  }

  let create: (string, string) => t
  let dummy: t
}

type t = {focus: ReactD3Graph.Node.t<Payload.t>}

module Kind: {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token
}

let create: (~name: string, ~reference: string, ~x: float, ~y: float, Kind.t) => t
let id: t => ReactD3Graph.Node.Id.t
let setSelected: (t, bool) => t
