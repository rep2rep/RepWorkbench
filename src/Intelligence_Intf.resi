module Request: {
  type t = State.Model.t
}

module Response: {
  type t = {
    warnings: array<ModelWarning.t>,
    errors: array<ModelError.t>,
  }
}

type t
type request = Request.t
type response = Response.t

let create: string => t
let listen: (t, response => unit) => unit
let post: (t, request) => unit

module WorkerThread: {
  @inline let create: (request => response) => unit
}
