module type Intf = {
  type t
  type request
  type response

  let create: string => t
  let listen: (t, response => unit) => unit
  let post: (t, request) => unit

  module WorkerThread: {
    let create: (request => response) => unit
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
(Intf with type request = Request.t and type response = Response.t) = (
  Request: Request_Intf,
  Response: Response_Intf,
) => {
  type t
  type request = Request.t
  type response = Response.t

  type message
  @get external data: message => string = "data"

  @new external create: string => t = "Worker"
  @send external addEventListener: (t, string, message => unit) => unit = "addEventListener"
  let listen = (t, callback) =>
    addEventListener(t, "message", msg =>
      msg->data->Js.Json.parseExn->Response.fromJson->Or_error.iter(callback)
    )
  @send external postMessage: (t, string) => unit = "postMessage"
  let post = (t, request) => {
    let req = Request.toJson(request)->Js.Json.stringify
    Js.Console.log(req)
    t->postMessage(req)
  }

  module WorkerThread = {
    type this
    @val external this: this = "this"
    @set external onmessage: (this, message => unit) => unit = "onmessage"
    @val external postMessage: string => unit = "postMessage"
    @inline
    let create = callback =>
      this->onmessage(msg =>
        msg
        ->data
        ->Js.Json.parseExn
        ->Request.fromJson
        ->Or_error.iter(request => {
          Js.Console.log(request)
          let resp = request->callback->Response.toJson->Js.Json.stringify
          console.log(resp)
          postMessage(resp)
        })
      )
  }
}
