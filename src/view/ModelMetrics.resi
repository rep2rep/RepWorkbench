type t

let hash: t => Hash.t
let isValid: t => Result.t<unit, array<string>>

let empty: t

let add: (t, string, string) => t
let addMany: (t, array<(string, string)>) => t

let results: t => array<(string, string)>
let toCSV: t => string

module Stable: {
  module V1: {
    type t = t
    let toJson: t => Js.Json.t
    let fromJson: Js.Json.t => Or_error.t<t>
  }
}
