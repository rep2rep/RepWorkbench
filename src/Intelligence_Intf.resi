module Request: {
  type t = {
    id: Gid.t,
    model: Gid.t,
    slots: Gid.Map.t<InspectorState.Schema.t>,
    links: array<(Gid.t, Gid.t, ModelLink.Kind.t)>,
  }
}

module Response: {
  type t = {
    id: Gid.t,
    model: Gid.t,
    warnings: array<ModelWarning.t>,
    warnings_done: bool,
    errors: array<ModelError.t>,
    errors_done: bool,
    insights: array<ModelInsight.t>,
    insights_done: bool,
    killed: bool,
  }

  let empty: t
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

type t
type request = Request.t
type response = Response.t

let create: string => t
let listen: (t, response => unit) => unit
let post: (t, request) => unit
let terminate: t => unit

module WorkerThread: {
  @inline let create: (request => response) => unit
  @inline let listen: (request => unit) => unit
  @inline let respond: response => unit
}
