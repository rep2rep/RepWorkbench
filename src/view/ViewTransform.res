include ReactD3Graph.Graph.ViewTransform

module Stable = {
  module V1 = {
    let toJson = t =>
      Js.Dict.fromArray([
        ("version", Int.toJson(1)),
        ("x", t->x->Float.toJson),
        ("y", t->y->Float.toJson),
        ("k", t->k->Float.toJson),
      ])->Js.Json.object_

    let fromJson = json =>
      json
      ->Js.Json.decodeObject
      ->Or_error.fromOption_s("Failed to decode GraphNode object JSON")
      ->Or_error.flatMap(dict => {
        let getValue = (key, reader) =>
          dict
          ->Js.Dict.get(key)
          ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
          ->Or_error.flatMap(reader)
        let version = getValue("version", Int.fromJson)
        switch version->Or_error.match {
        | Or_error.Ok(1) => {
            let x = getValue("x", Float.fromJson)
            let y = getValue("y", Float.fromJson)
            let k = getValue("k", Float.fromJson)
            (x, y, k)
            ->Or_error.both3
            ->Or_error.map(((x, y, k)) => init->translate(~x, ~y)->scale(k))
          }
        | Or_error.Ok(v) =>
          Or_error.error_ss(["Unknown version ", Int.toString(v), " for ViewTransform"])
        | Or_error.Err(e) => Or_error.error(e)
        }
      })
  }
}
