type t =
  | Semantic
  | Auxiliary
  | Arbitrary

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => option<t>
