type t<'a>

let create: unit => t<'a>

let set: (t<'a>, 'a) => unit
let get: t<'a> => option<'a>
let getExn: t<'a> => 'a
