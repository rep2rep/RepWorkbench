type t =
  | Nominal
  | Ordinal
  | Interval
  | Ratio

let toJson = t =>
  switch t {
  | Nominal => String.toJson("Nominal")
  | Ordinal => String.toJson("Ordinal")
  | Interval => String.toJson("Interval")
  | Ratio => String.toJson("Ratio")
  }

let fromJson = json =>
  switch String.fromJson(json) {
  | Some("Nominal") => Some(Nominal)
  | Some("Ordinal") => Some(Ordinal)
  | Some("Interval") => Some(Interval)
  | Some("Ratio") => Some(Ratio)
  | Some(_) => None
  | None => None
  }
