include module type of Js.String2

let toJson: string => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<string>

let approximateEmWidth: string => float
