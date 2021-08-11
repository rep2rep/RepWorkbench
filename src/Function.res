type t =
  | Semantic
  | Auxiliary
  | Arbitrary

let to_JSON = t =>
  switch t {
  | Semantic => Js.Json.string("Semantic")
  | Auxiliary => Js.Json.string("Auxiliary")
  | Arbitrary => Js.Json.string("Arbitrary")
  }

let of_JSON = json =>
  switch Js.Json.decodeString(json) {
  | Some("Semantic") => Some(Semantic)
  | Some("Auxiliary") => Some(Auxiliary)
  | Some("Arbitrary") => Some(Arbitrary)
  | Some(_) => None
  | None => None
  }
