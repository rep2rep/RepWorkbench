type t = Global | Local

let toString = t =>
  switch t {
  | Global => "Global"
  | Local => "Local"
  }

let fromString = s =>
  switch s {
  | "Global" => Some(Global)
  | "Local" => Some(Local)
  | _ => None
  }

let toJson = t => t->toString->String.toJson

let fromJson = json =>
  json
  ->String.fromJson
  ->Or_error.flatMap(s =>
    s->fromString->Or_error.fromOption_ss(["Scope '", s, "' is not one of Global or Local"])
  )

let all = [Global, Local]
