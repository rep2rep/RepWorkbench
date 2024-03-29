module Kind: {
  type t =
    | Representation
    | Scheme
    | Dimension
    | Token
    | Placeholder

  module Stable: {
    module V1: {
      type t
    }

    module V2: {
      type t = t
      let v1_to_v2: V1.t => t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

module Payload: {
  type t = {
    kind: Kind.t,
    name: string,
    name_suffix: option<string>,
    reference: string,
    reference_suffix: option<string>,
    dashed: bool,
  }

  module Stable: {
    module V1: {
      type t = {
        kind: Kind.Stable.V1.t,
        name: string,
        name_suffix: option<string>,
        reference: string,
        reference_suffix: option<string>,
        dashed: bool,
      }
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
    module V2: {
      type t = t = {
        kind: Kind.Stable.V2.t,
        name: string,
        name_suffix: option<string>,
        reference: string,
        reference_suffix: option<string>,
        dashed: bool,
      }
      let v1_to_v2: V1.t => t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

type t

let hash: t => Hash.t
let data: t => array<ReactD3Graph.Node.t<Payload.t>>
let isValid: t => Result.t<unit, array<string>>

let create: (~name: string, ~reference: string, ~x: float, ~y: float, Kind.t, Gid.t) => t
let dupWithNewId: (t, Gid.t) => t
let id: t => Gid.t
let kind: t => Kind.t
let payload: t => Payload.t
let position: t => (float, float)
let setPosition: (t, ~x: float, ~y: float) => t
let updatePayload: (t, Payload.t => Payload.t) => t

module Stable: {
  module V1: {
    type t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
  module V2: {
    type t = t
    let v1_to_v2: V1.t => t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
