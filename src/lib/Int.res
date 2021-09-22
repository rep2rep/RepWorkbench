include Belt.Int

@send external baseEncode: (int, int) => string = "toString"
@val external baseDecode: (string, int) => int = "parseInt"

let toJson = t => toFloat(t)->Js.Json.number
let fromJson = j => Js.Json.decodeNumber(j)->Option.map(fromFloat)
