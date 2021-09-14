type t =
  | Semantic
  | Auxiliary
  | Arbitrary

let toJson = t =>
  switch t {
  | Semantic => String.toJson("Semantic")
  | Auxiliary => String.toJson("Auxiliary")
  | Arbitrary => String.toJson("Arbitrary")
  }

let fromJson = json =>
  switch String.fromJson(json) {
  | Some("Semantic") => Some(Semantic)
  | Some("Auxiliary") => Some(Auxiliary)
  | Some("Arbitrary") => Some(Arbitrary)
  | Some(_) => None
  | None => None
  }
