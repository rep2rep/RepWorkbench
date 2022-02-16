type t<'a>

let create: int => t<'a>
let isEmpty: t<'a> => bool
let push: (t<'a>, 'a) => t<'a>
let pop: t<'a> => option<('a, t<'a>)>
let replaceTop: (t<'a>, 'a) => t<'a>
