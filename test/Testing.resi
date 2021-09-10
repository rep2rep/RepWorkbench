let runAll: unit => unit

let expect: (string, unit => 'a, string) => unit
let equal: (string, unit => 'a, 'a) => unit
let assertTrue: (string, unit => bool) => unit
let assertFalse: (string, unit => bool) => unit
