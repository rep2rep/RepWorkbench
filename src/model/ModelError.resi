type t

let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>

let create: (
  ~nodes: array<Gid.t>,
  ~message: string,
  ~details: string,
  ~suggestion: string=?,
  unit,
) => t

let id: t => Gid.t
let nodes: t => array<Gid.t>
let message: t => string
let details: t => string
let suggestion: t => option<string>

// Some useful errors

let internalError: (~code: string, ~details: string) => t
let cyclesError: t
let noRootError: t
let needsParentError: array<Gid.t> => t
let noFunctionError: array<Gid.t> => t
let noExplicitError: array<Gid.t> => t
let noScopeError: array<Gid.t> => t
let noQuantityScaleError: (array<Gid.t>, [#concept | #graphic]) => t
let unexpectedAnchorsError: (array<Gid.t>, [#representation | #scheme | #dimension]) => t
let badHierarchyError: (
  array<Gid.t>,
  ~parent: [#representation | #scheme | #dimension | #token],
  ~child: [#representation | #scheme | #dimension | #token],
) => t
