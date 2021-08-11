type t =
  | Nominal
  | Ordinal
  | Interval
  | Ratio

let to_JSON = t =>
  switch t {
  | Nominal => Js.Json.string("Nominal")
  | Ordinal => Js.Json.string("Ordinal")
  | Interval => Js.Json.string("Interval")
  | Ratio => Js.Json.string("Ratio")
  }

let of_JSON = json =>
  switch Js.Json.decodeString(json) {
  | Some("Nominal") => Some(Nominal)
  | Some("Ordinal") => Some(Ordinal)
  | Some("Interval") => Some(Interval)
  | Some("Ratio") => Some(Ratio)
  | Some(_) => None
  | None => None
  }
