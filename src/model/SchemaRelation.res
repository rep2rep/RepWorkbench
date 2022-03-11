module Kind = {
  type t =
    | Identical
    | Equivalent
    | Parallel
    | Other(string)

  let toJson = t =>
    switch t {
    | Identical => "Identical"
    | Equivalent => "Equivalent"
    | Parallel => "Parallel"
    | Other(s) => s
    }->String.toJson

  let fromJson = json =>
    json
    ->String.fromJson
    ->Or_error.map(s =>
      switch s {
      | "Identical" => Identical
      | "Equivalent" => Equivalent
      | "Parallel" => Parallel
      | other => Other(other)
      }
    )
}

type t = {
  first: Schema.t,
  second: Schema.t,
  kind: Kind.t,
}

let schema = t => (t.first, t.second)
let kind = t => t.kind

let toJson = t =>
  Js.Dict.fromList(list{
    ("first", t.first->Schema.uuid->Gid.toJson),
    ("second", t.second->Schema.uuid->Gid.toJson),
    ("kind", t.kind->Kind.toJson),
  })->Js.Json.object_

let fromJson = (json, root) =>
  Js.Json.decodeObject(json)
  ->Or_error.fromOption_s("JSON is not a valid object (reading SchemaRelation.t)")
  ->Or_error.flatMap(dict => {
    let first =
      dict
      ->Js.Dict.get("first")
      ->Or_error.fromOption_s("Unable to find first part of schema relation")
      ->Or_error.flatMap(Gid.fromJson)
      ->Or_error.flatMap(uuid =>
        root
        ->Schema.findByGid(uuid)
        ->Or_error.fromOption_s("First part of schema relation is not in supplied model")
      )
    let second =
      dict
      ->Js.Dict.get("second")
      ->Or_error.fromOption_s("Unable to find second part of schema relation")
      ->Or_error.flatMap(Gid.fromJson)
      ->Or_error.flatMap(uuid =>
        root
        ->Schema.findByGid(uuid)
        ->Or_error.fromOption_s("First part of schema relation is not in supplied model")
      )

    let kind =
      dict
      ->Js.Dict.get("kind")
      ->Or_error.fromOption_s("Unable to find second part of schema relation")
      ->Or_error.flatMap(Kind.fromJson)

    Or_error.both3((first, second, kind))->Or_error.map(((first, second, kind)) => {
      first: first,
      second: second,
      kind: kind,
    })
  })
