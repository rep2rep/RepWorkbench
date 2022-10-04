type t = {
  id: Gid.t,
  nodes: array<Gid.t>,
  message: string,
  details: string,
  suggestion: option<string>,
}

let toJson = t =>
  Js.Dict.fromList(list{
    ("id", t.id->Gid.toJson),
    ("nodes", t.nodes->Array.toJson(Gid.toJson)),
    ("message", t.message->String.toJson),
    ("details", t.details->String.toJson),
    ("suggestion", t.suggestion->Option.toJson(String.toJson)),
  })->Js.Json.object_

let fromJson = json =>
  json
  ->Js.Json.decodeObject
  ->Or_error.fromOption_s("Unable to parse object for ModelError")
  ->Or_error.flatMap(dict => {
    let getValue = (key, reader) =>
      dict
      ->Js.Dict.get(key)
      ->Or_error.fromOption_ss(["Unable to find key '", key, "'"])
      ->Or_error.flatMap(reader)
    let id = getValue("id", Gid.fromJson)
    let nodes = getValue("nodes", Array.fromJson(_, Gid.fromJson))
    let message = getValue("message", String.fromJson)
    let details = getValue("details", String.fromJson)
    let suggestion = getValue("suggestion", Option.fromJson(_, String.fromJson))

    Or_error.both5((id, nodes, message, details, suggestion))->Or_error.map(((
      id,
      nodes,
      message,
      details,
      suggestion,
    )) => {
      id: id,
      nodes: nodes,
      message: message,
      details: details,
      suggestion: suggestion,
    })
  })

let stableId = (nodes, message) => {
  let msg =
    message
    ->Js.String2.castToArrayLike
    ->Js.Array2.fromMap(s => s->Js.String2.codePointAt(0)->Option.getExn)
    ->Array.reduceWithIndex(0, (b, a, i) =>
      if mod(i, 5) === 0 && i < 60 {
        2 * b + (a - 32)
      } else {
        b
      }
    )
    ->Int.toString
  Gid.fromString(Gid.combine(nodes)->Gid.toString ++ msg ++ "1")
}

let create = (~nodes, ~message, ~details, ~suggestion=?, ()) => {
  id: stableId(nodes, message),
  nodes: nodes,
  message: message,
  details: details,
  suggestion: suggestion,
}
let id = t => t.id
let nodes = t => t.nodes
let message = t => t.message
let details = t => t.details
let suggestion = t => t.suggestion

// Some useful errors

let kindToString = kind =>
  switch kind {
  | #representation => "Representation"
  | #scheme => "R-Scheme"
  | #dimension => "R-Dimension"
  | #token => "R-Symbol"
  | #placeholder => "Placeholder"
  }

let internalError = (~code, ~details) =>
  create(
    ~nodes=[],
    ~message="Internal Error: " ++ code,
    ~details,
    ~suggestion="Notify Aaron about this so he can fix it :-) ",
    (),
  )

let needsParentError = nodes =>
  create(
    ~nodes,
    ~message="Schema has no parent, but it needs one.",
    ~details="All schemas, except for Representation schemas, must either be in the hierarchy, or be anchored below a R-symbol in the hierarchy. This schema is neither in the hierarchy, nor anchored.",
    ~suggestion="Connect this schema below another schema.",
    (),
  )

let noFunctionError = nodes =>
  create(
    ~nodes,
    ~message="Missing \"Function\" of schema.",
    ~details="This schema requires a \"Function\", whether it is semantic, auxiliary, or arbitrary. This has not been set.",
    ~suggestion="Select the appropriate \"Function\" from the dropdown menu.",
    (),
  )

let noExplicitError = nodes =>
  create(
    ~nodes,
    ~message="Missing whether schema is \"Explicit\".",
    ~details="This schema needs to be marked as \"Explicit\", or not, depending on whether it is explicit in the representation. This has not been set.",
    ~suggestion="Select the appropriate \"Explicit\" value (Yes or No) from the dropdown menu.",
    (),
  )

let noScopeError = nodes =>
  create(
    ~nodes,
    ~message="Missing the \"Scope\" of the schema.",
    ~details="This schema is either \"Global\" or \"Local\" in \"Scope\". This has not been set.",
    ~suggestion="Select the appropriate \"Scope\" value (Global or Local) from the dropdown menu.",
    (),
  )

let noQuantityScaleError = (nodes, kind) => {
  let kind = switch kind {
  | #concept => "Concept"
  | #graphic => "Graphic"
  }
  create(
    ~nodes,
    ~message="Missing the \"" ++ kind ++ " Scale\" of the R-dimension.",
    ~details="This R-dimension's \"" ++
    kind ++ " Scale\" must be one of \"Nominal\", \"Ordinal\", \"Interval\", or \"Ratio\". This has not been set.",
    ~suggestion="Select the appropriate \"" ++ kind ++ " Scale\" value from the dropdown menu.",
    (),
  )
}
let cyclesError = create(
  ~nodes=[],
  ~message="Model has a cycle.",
  ~details="Models should not contain cycles: that is, the hierarchy and anchoring links should always connect a higher schema to a lower schema. We have found a situation where a lower schema is connected to a higher schema.",
  ~suggestion="We've detected a cycle. Find the link going the 'wrong way', and remove it.",
  (),
)
let noRootError = create(
  ~nodes=[],
  ~message="Could not determine root of model.",
  ~details="Each model should have a root. Somehow, we failed to find a root!",
  (),
)

let unexpectedAnchorsError = (nodes, kind) => {
  let kind = kindToString(kind)
  create(
    ~nodes,
    ~message={kind ++ " schema has unexpected anchored children."},
    ~details={
      "Anchoring is a type of link that is uniquely below R-symbol schemas. However, this node is a " ++
      kind ++ " schema."
    },
    ~suggestion="(1) Make this schema an R-symbol. (2) Connect the children using hierarchy. (3) Remove the children.",
    (),
  )
}

let badHierarchyError = (nodes, ~parent, ~child) => {
  let parent = kindToString(parent)
  let child = kindToString(child)
  let childArticle = if child->String.startsWith("R-") {
    "an"
  } else {
    "a"
  }
  create(
    ~nodes,
    ~message=parent ++ " has " ++ childArticle ++ " " ++ child ++ " below it.",
    ~details=parent ++ " schemas cannot have " ++ child ++ " schemas as direct descendants.",
    ~suggestion="Remove this " ++ child ++ " schema, or connect it elsewhere in the model.",
    (),
  )
}
