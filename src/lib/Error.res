module Message: {
  type t

  let fromString: string => t
  let toString: t => string

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
} = {
  type t = string

  let fromString = s => s
  let toString = t => t

  let toJson = Js.Json.string
  let fromJson = Js.Json.decodeString
}

module Tag: {
  type t

  let fromString: string => t
  let toString: t => string

  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => option<t>
} = {
  type t = string

  let fromString = s => s
  let toString = t => Js.String2.concat("TAG: ", t)

  let toJson = Js.Json.string
  let fromJson = Js.Json.decodeString
}

module MessageOrTag = {
  type t =
    | Message(Message.t)
    | Tag(Tag.t)

  let toJson = t => {
    let (label, json) = switch t {
    | Message(m) => ("Message", Message.toJson(m))
    | Tag(t) => ("Tag", Tag.toJson(t))
    }
    Js.Json.array([Js.Json.string(label), json])
  }

  let fromJson = json =>
    switch Js.Json.decodeArray(json) {
    | Some([label, json]) =>
      switch Js.Json.decodeString(label) {
      | Some("Message") => Message.fromJson(json)->Belt.Option.map(m => Message(m))
      | Some("Tag") => Tag.fromJson(json)->Belt.Option.map(t => Tag(t))
      | _ => None
      }
    | Some(_) => None
    | None => None
    }
}

type t = array<MessageOrTag.t>

let join = Js.Array2.concat
let concat = ts => Belt.List.reduce(ts, [], join)

let tag = (t, s) => t->join([MessageOrTag.Tag(Tag.fromString(s))])

let tags = t => t->Js.Array2.reduce((a, x) =>
    switch x {
    | MessageOrTag.Tag(t) => Js.Array2.concat(a, [t])
    | _ => a
    }
  , [])

let messages = t => t->Js.Array2.reduce((a, x) =>
    switch x {
    | MessageOrTag.Message(m) => Js.Array2.concat(a, [Message.toString(m)])
    | _ => a
    }
  , [])

let toString_ = (t, includeTags) =>
  Js.String2.concatMany(
    "\n",
    t->Js.Array2.map(s =>
      switch s {
      | MessageOrTag.Message(m) => Message.toString(m)->Js.String2.concat("\n")
      | MessageOrTag.Tag(t) =>
        if includeTags {
          Tag.toString(t)->Js.String2.concat("\n")
        } else {
          ""
        }
      }
    ),
  )
let toString = t => toString_(t, false)
let toStringWithTags = t => toString_(t, true)
let fromString = s => [MessageOrTag.Message(Message.fromString(s))]
let fromStrings = ss => [MessageOrTag.Message(Js.String2.concatMany("", ss)->Message.fromString)]

let raise_ = t => Js.Exn.raiseError(toString(t))
let raiseWithTags = t => Js.Exn.raiseError(toStringWithTags(t))

let toJson = t => t->Js.Array2.map(MessageOrTag.toJson)->Js.Json.array
let fromJson = json =>
  json
  ->Js.Json.decodeArray
  ->Belt.Option.flatMap(arr =>
    arr
    ->Js.Array2.map(MessageOrTag.fromJson)
    ->Js.Array2.reduce(
      (a, x) => a->Belt.Option.flatMap(a => x->Belt.Option.map(x => Js.Array2.concat(a, [x]))),
      Some([]),
    )
  )

let _pretag = (t, tags) => join(tags->Js.Array2.map(t => MessageOrTag.Tag(t)), t)
let _posttag = (t, tags) => join(t, tags->Js.Array2.map(t => MessageOrTag.Tag(t)))
