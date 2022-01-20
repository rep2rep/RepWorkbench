type key = string
type data = string

let setItem: (key, data) => unit
let getItem: key => option<data>
let removeItem: key => unit
let clear: unit => unit
let length: unit => int
