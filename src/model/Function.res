type t =
  | Semantic
  | Auxiliary
  | Arbitrary

let toJson = t =>
  switch t {
  | Semantic => "Semantic"
  | Auxiliary => "Auxiliary"
  | Arbitrary => "Arbitrary"
  }->String.toJson

let fromJson = json =>
  switch String.fromJson(json) {
  | Some("Semantic") => Some(Semantic)
  | Some("Auxiliary") => Some(Auxiliary)
  | Some("Arbitrary") => Some(Arbitrary)
  | Some(_) => None
  | None => None
  }
