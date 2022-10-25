type t = ReactD3Graph.Graph.Selection.t

let duplicate: (t, Gid.Map.t<Gid.t>) => t
let isValid: t => Result.t<unit, array<string>>

let empty: t
let ofNodes: array<Gid.t> => t

let nodes: t => array<Gid.t>
let links: t => array<Gid.t>

module Stable: {
  module V1: {
    type t = t

    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
