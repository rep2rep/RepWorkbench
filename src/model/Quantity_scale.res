type t =
  | Nominal
  | Ordinal
  | Interval
  | Ratio

let toString = t =>
  switch t {
  | Nominal => "Nominal"
  | Ordinal => "Ordinal"
  | Interval => "Interval"
  | Ratio => "Ratio"
  }

let fromString = s =>
  switch s {
  | "Nominal" => Some(Nominal)
  | "Ordinal" => Some(Ordinal)
  | "Interval" => Some(Interval)
  | "Ratio" => Some(Ratio)
  | _ => None
  }

let toJson = t => t->toString->String.toJson

let fromJson = json =>
  json
  ->String.fromJson
  ->Or_error.flatMap(s =>
    s
    ->fromString
    ->Or_error.fromOption_ss([
      "Quantity scale '",
      s,
      "' is not one of Nominal, Ordinal, Interval, or Ratio",
    ])
  )

let all = [Nominal, Ordinal, Interval, Ratio]
