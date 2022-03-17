type t = {
  id: Gid.t,
  nodes: array<Gid.t>,
  message: string,
  details: string,
  suggestion: option<string>,
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("id", t.id->Gid.toJson),
    ("nodes", t.nodes->Array.toJson(Gid.toJson)),
    ("message", t.message->String.toJson),
    ("details", t.details->String.toJson),
    ("suggestion", t.suggestion->Option.toJson(String.toJson)),
  })->Js.Json.object_

let fromJson = json =>
  json
  ->Js.Json.decodeObject
  ->Or_error.fromOption_s("Unable to parse object for ModelError")
  ->Or_error.flatMap(dict => {
    let getValue = (key, reader) =>
      dict
      ->Js.Dict.get(key)
      ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
      ->Or_error.flatMap(reader)
    let id = getValue("id", Gid.fromJson)
    let nodes = getValue("nodes", Array.fromJson(_, Gid.fromJson))
    let message = getValue("message", String.fromJson)
    let details = getValue("details", String.fromJson)
    let suggestion = getValue("suggestion", Option.fromJson(_, String.fromJson))

    Or_error.both5((id, nodes, message, details, suggestion))->Or_error.map(((
      id,
      nodes,
      message,
      details,
      suggestion,
    )) => {
      id: id,
      nodes: nodes,
      message: message,
      details: details,
      suggestion: suggestion,
    })
  })

let stableId = (nodes, message) => {
  let msg =
    message
    ->Js.String2.castToArrayLike
    ->Js.Array2.fromMap(s => s->Js.String2.codePointAt(0)->Option.getExn)
    ->Array.reduceWithIndex(0, (b, a, i) =>
      if mod(i, 5) === 0 && i < 60 {
        2 * b + (a - 32)
      } else {
        b
      }
    )
    ->Int.toString
  Gid.fromString(Gid.combine(nodes)->Gid.toString ++ msg ++ "1")
}

let create = (~nodes, ~message, ~details, ~suggestion=?, ()) => {
  id: stableId(nodes, message),
  nodes: nodes,
  message: message,
  details: details,
  suggestion: suggestion,
}
let id = t => t.id
let nodes = t => t.nodes
let message = t => t.message
let details = t => t.details
let suggestion = t => t.suggestion
