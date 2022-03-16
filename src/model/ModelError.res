type t = {
  id: Gid.t,
  node: Gid.t,
  message: string,
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("id", t.id->Gid.toJson),
    ("node", t.node->Gid.toJson),
    ("message", t.message->String.toJson),
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

    Or_error.both3((id, node, message))->Or_error.map(((id, node, message)) => {
      id: id,
      node: node,
      message: message,
    })
  })

let stableId = (node, message) => {
  let idify = s =>
    s
    ->Js.String2.castToArrayLike
    ->Js.Array2.fromMap(s => s->Js.String2.codePointAt(0)->Option.getExn)
    ->Js.Array2.reduce((a, b) => 2 * a + (b - 40) + 1, 0)
    ->Int.toString
    ->Gid.fromString
  idify("0" ++ idify(message)->Gid.toString ++ idify(Gid.toString(node))->Gid.toString)
}

let create = (node, message) => {id: stableId(node, message), node: node, message: message}
let id = t => t.id
let node = t => t.node
let message = t => t.message
