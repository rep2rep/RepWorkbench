type t = {
  root: Schema.t,
  relations: list<SchemaRelation.t>,
}

let root = t => t.root
let relations = t => t.relations
let validate = t => Schema.validate(t.root)

let toJson = t =>
  Js.Dict.fromList(list{
    ("root", t.root->Schema.toJson),
    ("relations", t.relations->List.toJson(SchemaRelation.toJson)),
  })->Js.Json.object_

let fromJson = json =>
  Js.Json.decodeObject(json)
  ->Or_error.fromOption_s("JSON is not a valid object (reading Model.t)")
  ->Or_error.flatMap(dict => {
    let root =
      dict
      ->Js.Dict.get("root")
      ->Or_error.fromOption_s("Unable to find model root (reading Model.t)")
      ->Or_error.flatMap(Schema.fromJson)
    let relations =
      dict
      ->Js.Dict.get("relations")
      ->Or_error.fromOption_s("Unable to find model relations (reading Model.t)")
      ->Or_error.flatMap(l =>
        l->List.fromJson(j => root->Or_error.flatMap(root => j->SchemaRelation.fromJson(root)))
      )

    Or_error.both((root, relations))->Or_error.map(((root, relations)) => {
      root: root,
      relations: relations,
    })
  })