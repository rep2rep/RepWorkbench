type t = Atomic | Expression

let toJson = t =>
  switch t {
  | Atomic => Js.Json.string("Atomic")
  | Expression => Js.Json.string("Expression")
  }

let fromJson = json =>
  switch Js.Json.decodeString(json) {
  | Some("Atomic") => Or_error.create(Atomic)
  | Some("Expression") => Or_error.create(Expression)
  | _ => Or_error.error_s("Token level is not one of Atomic or Expression")
  }
