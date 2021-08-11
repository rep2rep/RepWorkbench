type t<'a>

let of_list: list<'a> => option<t<'a>>
let to_list: t<'a> => list<'a>

let cons: (t<'a>, 'a) => t<'a>
let head: t<'a> => 'a
let length: t<'a> => int
let map: (t<'a>, 'a => 'b) => t<'b>
let reduce: (t<'a>, ('a, 'a) => 'a) => 'a
let foldr: (t<'a>, 'b, ('a, 'b) => 'b) => 'b
let every: (t<'a>, 'a => bool) => bool
