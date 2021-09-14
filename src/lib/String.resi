include module type of Js.String2

let toJson: string => Js.Json.t
let fromJson: Js.Json.t => option<string>
