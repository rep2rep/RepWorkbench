module Request: {
  type t = {
    id: Gid.t,
    slots: Gid.Map.t<InspectorState.Schema.t>,
    links: array<(Gid.t, Gid.t, ModelLink.Kind.t)>,
  }
}

module Response: {
  type t = {
    id: Gid.t,
    warnings: array<ModelWarning.t>,
    errors: array<ModelError.t>,
    insights: array<ModelInsight.t>,
  }

  let empty: t
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
