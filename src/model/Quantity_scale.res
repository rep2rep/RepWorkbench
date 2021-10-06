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
  switch String.fromJson(json)->Or_error.valOf {
  | Some("Nominal") => Or_error.create(Nominal)
  | Some("Ordinal") => Or_error.create(Ordinal)
  | Some("Interval") => Or_error.create(Interval)
  | Some("Ratio") => Or_error.create(Ratio)
  | Some(_) => Or_error.error_s("Quantity scale is not one of Nominal, Ordinal, Interval, or Ratio")
  | None => Or_error.error_s("Unable to decode string (reading Quantity_scale.t)")
  }
