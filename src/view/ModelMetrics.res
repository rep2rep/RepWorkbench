type t = array<(string, string)>

let hash = t => t->Array.hash(((a, b)) => Array.hash([a, b], String.hash))
let isValid = _ => Result.Ok()

let empty = []
let add = (t, k, v) => Array.push(t, (k, v))
let addMany = Array.concat

let results = t => t
let toCSV = t =>
  "data:text/csv;charset=utf-8," ++
  t->Array.map(((k, v)) => k ++ "," ++ v)->Array.joinWith("\r\n")->Js.Global.encodeURIComponent

module Stable = {
  module V1 = {
    type t = array<(string, string)>

    let toJson = t => t->Array.toJson(((a, b)) => Array.toJson([a, b], String.toJson))
    let fromJson = json =>
      json->Array.fromJson(j =>
        j
        ->Array.fromJson(String.fromJson)
        ->Or_error.flatMap(arr =>
          switch arr {
          | [a, b] => Or_error.create((a, b))
          | _ => Or_error.error_s("ModelMetrics malformed.")
          }
        )
      )
  }
}
