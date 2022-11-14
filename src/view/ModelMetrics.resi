type t

let hash: t => Hash.t
let isValid: t => Result.t<unit, array<string>>

let empty: t

let compute: ModelState.t => Promise.t<t>

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
