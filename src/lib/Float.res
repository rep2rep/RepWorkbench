include Belt.Float

let toJson = Js.Json.number
let fromJson = j =>
  Js.Json.decodeNumber(j)->Or_error.fromOption_s("JSON is not a number (reading Float)")

let max = (t, t') =>
  if t >= t' {
    t
  } else {
    t'
  }
let min = (t, t') =>
  if t <= t' {
    t
  } else {
    t'
  }
