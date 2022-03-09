module type Intf = {
  type t
  type request
  type response

  let create: string => t
  let listen: (t, response => unit) => unit
  let post: (t, request) => unit

  module WorkerThread: {
    @inline let create: (request => response) => unit
  }
}

module type Request_Intf = {
  type t
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}
module type Response_Intf = {
  type t
  let toJson: t => Js.Json.t
  let fromJson: Js.Json.t => Or_error.t<t>
}

module Make: (Request: Request_Intf, Response: Response_Intf) =>
(Intf with type request = Request.t and type response = Response.t)
