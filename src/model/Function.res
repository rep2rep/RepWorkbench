type t =
  | Semantic
  | Auxiliary
  | Arbitrary

let toJson = t =>
  switch t {
  | Semantic => Js.Json.string("Semantic")
  | Auxiliary => Js.Json.string("Auxiliary")
  | Arbitrary => Js.Json.string("Arbitrary")
  }

let fromJson = json =>
  switch Js.Json.decodeString(json) {
  | Some("Semantic") => Some(Semantic)
  | Some("Auxiliary") => Some(Auxiliary)
  | Some("Arbitrary") => Some(Arbitrary)
  | Some(_) => None
  | None => None
  }
