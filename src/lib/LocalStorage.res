module type Stringable = {
  type t

  let toString: t => string
  let fromString: string => option<t>
}

module type Jsonable = {
  type t

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

module Raw = {
  type key = string
  type data = string

  type t = {length: int}
  @val external t: t = "localStorage"

  @send external _setItem: (t, key, data) => unit = "setItem"
  @send external _getItem: (t, key) => Js.Nullable.t<data> = "getItem"
  @send external _removeItem: (t, key) => unit = "removeItem"
  @send external _clear: (t, unit) => unit = "clear"

  let setItem = _setItem(t)
  let getItem = key => _getItem(t, key)->Js.Nullable.toOption
  let removeItem = _removeItem(t)
  let clear = _clear(t)
  let length = () => t.length
}

module MakeStringable = (S: Stringable) => {
  type key = string

  let set = (key, t) => Raw.setItem(key, S.toString(t))
  let get = key => Raw.getItem(key)->Option.flatMap(S.fromString)
  let delete = key => Raw.removeItem(key)
}

module MakeJsonable = (S: Jsonable) => {
  type key = string

  let set = (key, t) => Raw.setItem(key, S.toJson(t)->Js.Json.stringify)
  let get = key =>
    switch Raw.getItem(key) {
    | None => Or_error.error_ss(["Unable to retrieve from storage '", key, "'"])
    | Some(s) =>
      try Or_error.create(Js.Json.parseExn(s)) catch {
      | _ => Or_error.error_s("Failed to load state")
      }->Or_error.flatMap(S.fromJson)
    }
  let delete = key => Raw.removeItem(key)
}
