include module type of Belt.Int

let toJson: int => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<int>

let baseEncode: (int, int) => string
let baseDecode: (string, int) => int
