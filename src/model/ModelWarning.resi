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

// Some useful warnings
let defaultConceptWarning: (
  array<Gid.t>,
  ~field: string,
  ~default: string,
  [#representation | #scheme | #dimension | #token | #placeholder],
) => t
let defaultReferenceWarning: (
  array<Gid.t>,
  string,
  [#representation | #scheme | #dimension | #token | #placeholder],
) => t
let multipleRootsWarning: array<Gid.t> => t
