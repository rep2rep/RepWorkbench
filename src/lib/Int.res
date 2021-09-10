include Belt.Int

let toJson = t => toFloat(t)->Js.Json.number
let fromJson = j => Js.Json.decodeNumber(j)->Option.map(fromFloat)
