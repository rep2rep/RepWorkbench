module Action = {
  type act<'a, 'b> = {
    apply: 'a => 'b,
    unapply: 'b => 'a,
  }
  type t<'a> = act<'a, 'a>

  type packed

  external pack: act<'a, 'b> => packed = "%identity"
  external unpack: packed => act<'a, 'b> = "%identity"

  let create = (f, finv) => {apply: f, unapply: finv}
  let then_ = (t, t') => {
    apply: arg => arg->t.apply->t'.apply,
    unapply: res => res->t'.unapply->t.unapply,
  }
  let apply = (t, a) => t.apply(a)
  let unapply = (t, b) => t.unapply(b)
}

type t<'a> = {
  state: 'a,
  pastActions: array<Action.packed>,
  futureActions: array<Action.packed>,
}

let create = state => {state: state, pastActions: [], futureActions: []}
let state = t => t.state
let canUndo = t => t.pastActions != []
let canRedo = t => t.futureActions != []

let doAction = (t, action) => {
  state: action->Action.apply(t.state),
  pastActions: t.pastActions->Array.push(Action.pack(action)),
  futureActions: [],
}

let undo = t => {
  let (action, newPast) = t.pastActions->Array.pop
  {
    state: action->Action.unpack->Action.unapply(t.state),
    pastActions: newPast,
    futureActions: t.futureActions->Array.push(action),
  }
}

let redo = t => {
  let (action, newFuture) = t.futureActions->Array.pop
  {
    state: action->Action.unpack->Action.apply(t.state),
    pastActions: t.pastActions->Array.push(action),
    futureActions: newFuture,
  }
}
