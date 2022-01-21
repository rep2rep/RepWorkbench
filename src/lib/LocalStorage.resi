module type Stringable = {
  type t
  let key: string

  let toString: t => string
  let fromString: string => option<t>
}

module type Jsonable = {
  type t
  let key: string
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

module Raw: {
  type key = string
  type data = string

  let setItem: (key, data) => unit
  let getItem: key => option<data>
  let removeItem: key => unit
  let clear: unit => unit
  let length: unit => int
}

module MakeStringable: (S: Stringable) =>
{
  let set: S.t => unit
  let get: unit => option<S.t>
  let delete: unit => unit
}

module MakeJsonable: (S: Jsonable) =>
{
  let set: S.t => unit
  let get: unit => Or_error.t<S.t>
  let delete: unit => unit
}
