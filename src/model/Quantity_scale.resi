type t =
  | Nominal
  | Ordinal
  | Interval
  | Ratio

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>
