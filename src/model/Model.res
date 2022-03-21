type t = {
  root: Schema.t,
  relations: array<SchemaRelation.t>,
}

let root = t => t.root
let schemas = t => {
  let schemas = []
  let rec f = schema => {
    schemas->Js.Array2.push(schema)->ignore
    Schema.children(schema)->List.forEach(f)
  }
  f(t.root)
  Array.dedup(schemas)
}
let relations = t => t.relations
let validate = t =>
  switch t.root {
  | Schema.Representation(_) => Schema.validate(t.root)
  | Schema.Scheme(_) | Schema.Dimension(_) | Schema.Token(_) =>
    Or_error.both((
      Schema.validate(t.root),
      Or_error.error_s("Model root must be a Representation schema."),
    ))->Or_error.map(((s, _)) => s)
  }

let toJson = t =>
  Js.Dict.fromList(list{
    ("root", t.root->Schema.toJson),
    ("relations", t.relations->Array.toJson(SchemaRelation.toJson)),
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
        l->Array.fromJson(j => root->Or_error.flatMap(root => j->SchemaRelation.fromJson(root)))
      )

    Or_error.both((root, relations))->Or_error.map(((root, relations)) => {
      root: root,
      relations: relations,
    })
  })
