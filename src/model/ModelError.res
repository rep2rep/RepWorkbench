type t = {
  id: Gid.t,
  node: Gid.t,
  message: string,
  details: string,
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("id", t.id->Gid.toJson),
    ("node", t.node->Gid.toJson),
    ("message", t.message->String.toJson),
    ("details", t.details->String.toJson),
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
    let node = getValue("node", Gid.fromJson)
    let message = getValue("message", String.fromJson)
    let details = getValue("details", String.fromJson)

    Or_error.both4((id, node, message, details))->Or_error.map(((id, node, message, details)) => {
      id: id,
      node: node,
      message: message,
      details: details,
    })
  })

let stableId = (node, message) => {
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
  Gid.fromString(Gid.toString(node) ++ msg ++ "1")
}

let create = (~node, ~message, ~details) => {
  id: stableId(node, message),
  node: node,
  message: message,
  details: details,
}
let id = t => t.id
let node = t => t.node
let message = t => t.message
let details = t => t.details
