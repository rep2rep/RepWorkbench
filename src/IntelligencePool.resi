type t

let create: string => t

let post: (t, Intelligence_Intf.Request.t) => unit
let listen: (t, Intelligence_Intf.Response.t => unit) => unit
