type t = Global | Local

let toJson = t =>
  switch t {
  | Global => String.toJson("Global")
  | Local => String.toJson("Local")
  }

let fromJson = json =>
  switch String.fromJson(json) {
  | Some("Global") => Some(Global)
  | Some("Local") => Some(Local)
  | Some(_) => None
  | None => None
  }
