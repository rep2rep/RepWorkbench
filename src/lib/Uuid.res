type t = int

let base = 16

type internal = {mutable counter: int}

let internal = {counter: Js.Date.now()->Belt.Float.toInt}

let create = () => {
  let value = internal.counter
  internal.counter = internal.counter + 1
  value
}

let toString = t => Int.baseEncode(t, base)
let fromString = s => Int.baseDecode(s, base)

let toJson = t => toString(t)->String.toJson
let fromJson = j => String.fromJson(j)->Or_error.map(s => fromString(s))

module Cmp = Belt.Id.MakeComparable({
  type t = t
  let cmp = (t, t') =>
    if t > t' {
      1
    } else if t < t' {
      -1
    } else {
      0
    }
})

module Set = {
  type key = Cmp.t
  type id = Cmp.identity
  type t = Belt.Set.t<key, id>

  let empty = Belt.Set.make(~id=module(Cmp))

  let isEmpty = Belt.Set.isEmpty
  let has = Belt.Set.has
  let add = Belt.Set.add
  let union = Belt.Set.union
  let intersect = Belt.Set.intersect
  let toList = Belt.Set.toList
  let fromList = lst => lst->List.reduce(empty, (t, k) => add(t, k))

  let toJson = t => toList(t)->List.toJson(toJson)

  let fromJson = json => json->List.fromJson(fromJson)->Or_error.map(fromList)
}

module Map = {
  type key = Cmp.t
  type id = Cmp.identity
  type t<'v> = Belt.Map.t<key, 'v, id>

  let empty = () => Belt.Map.make(~id=module(Cmp))

  let isEmpty = Belt.Map.isEmpty
  let has = Belt.Map.has
  let set = Belt.Map.set
  let get = Belt.Map.get
  let update = Belt.Map.update
  let remove = Belt.Map.remove
  let map = Belt.Map.map
  let mapWithKey = Belt.Map.mapWithKey
  let forEach = Belt.Map.forEach
  let merge = Belt.Map.merge
  let keys = Belt.Map.keysToArray
  let values = Belt.Map.valuesToArray

  let toList = Belt.Map.toList
  let fromList = lst => lst->List.reduce(empty(), (t, (k, v)) => set(t, k, v))
  let toArray = Belt.Map.toArray
  let fromArray = Belt.Map.fromArray(~id=module(Cmp))

  let toJson = (t, encode) =>
    toList(t)->List.map(((k, v)) => (toString(k), encode(v)))->Js.Dict.fromList->Js.Json.object_

  let fromJson = (json, decode) =>
    Js.Json.decodeObject(json)
    ->Or_error.fromOption_s("JSON is not an object (reading UUID)")
    ->Or_error.flatMap(dict =>
      Js.Dict.entries(dict)
      ->Array.map(((k, v)) => decode(v)->Or_error.map(v => (fromString(k), v)))
      ->List.fromArray
      ->Or_error.all
      ->Or_error.map(fromList)
    )
}
