module type S = {
  type t
  let validate: t => bool
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
}
