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
  type key = string

  let set: (key, S.t) => unit
  let get: key => option<S.t>
  let delete: key => unit
}

module MakeJsonable: (S: Jsonable) =>
{
  type key = string

  let set: (key, S.t) => unit
  let get: key => Or_error.t<S.t>
  let delete: key => unit
}
