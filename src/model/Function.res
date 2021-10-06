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
  switch String.fromJson(json)->Or_error.valOf {
  | Some("Semantic") => Or_error.create(Semantic)
  | Some("Auxiliary") => Or_error.create(Auxiliary)
  | Some("Arbitrary") => Or_error.create(Arbitrary)
  | Some(_) => Or_error.error_s("Function is not one of Sematic, Auxiliary, or Arbitrary")
  | None => Or_error.error_s("Unable to decode string (reading Function.t)")
  }
