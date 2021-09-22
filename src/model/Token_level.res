type t = Atomic | Expression

let toJson = t =>
  switch t {
  | Atomic => Js.Json.string("Atomic")
  | Expression => Js.Json.string("Expression")
  }

let fromJson = json =>
  switch Js.Json.decodeString(json) {
  | Some("Atomic") => Some(Atomic)
  | Some("Expression") => Some(Expression)
  | _ => None
  }
