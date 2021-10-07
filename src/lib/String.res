include Js.String2

let toJson = Js.Json.string
let fromJson = j =>
  j->Js.Json.decodeString->Or_error.fromOption_ss(["Not a JSON string: ", make(j)])
