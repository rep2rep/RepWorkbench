type t =
  | Nominal
  | Ordinal
  | Interval
  | Ratio

let n_const = Hash.unique()
let o_const = Hash.unique()
let i_const = Hash.unique()
let r_const = Hash.unique()
let hash = t =>
  switch t {
  | Nominal => n_const
  | Ordinal => o_const
  | Interval => i_const
  | Ratio => r_const
  }

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
