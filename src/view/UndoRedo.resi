type t<'a>

module Action: {
  type act<'a, 'b>
  type t<'a>

  let create: ('a => 'b, 'b => 'a) => act<'a, 'b>
  let then_: (act<'a, 'b>, act<'b, 'c>) => act<'a, 'c>
  let apply: (act<'a, 'b>, 'a) => 'b
  let unapply: (act<'a, 'b>, 'b) => 'a
}

let create: 'a => t<'a>
let state: t<'a> => 'a

let canUndo: t<'a> => bool
let canRedo: t<'a> => bool

let doAction: (t<'a>, Action.t<'a>) => t<'a>
let undo: t<'a> => t<'a>
let redo: t<'a> => t<'a>
