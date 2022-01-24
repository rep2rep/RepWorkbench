type t =
  | Semantic
  | Auxiliary
  | Arbitrary

let toString = t =>
  switch t {
  | Semantic => "Semantic"
  | Auxiliary => "Auxiliary"
  | Arbitrary => "Arbitrary"
  }

let fromString = s =>
  switch s {
  | "Semantic" => Some(Semantic)
  | "Auxiliary" => Some(Auxiliary)
  | "Arbitrary" => Some(Arbitrary)
  | _ => None
  }

let toJson = t => t->toString->String.toJson

let fromJson = json =>
  String.fromJson(json)->Or_error.flatMap(s =>
    fromString(s)->Or_error.fromOption_ss([
      "Function '",
      s,
      "' is not one of Semantic, Auxiliary, or Arbitrary",
    ])
  )

let all = [Semantic, Auxiliary, Arbitrary]
