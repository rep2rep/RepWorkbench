type t<'a> = {
  state: 'a,
  stepped_at: float,
  past: BoundedArray.t<(float, 'a)>,
  future: BoundedArray.t<(float, 'a)>,
}

let history_limit = 50
let coalesce_limit = 200.0

type performance
@val external performance: performance = "performance"
@send external perfNow: performance => float = "now"

let create = state => {
  state: state,
  stepped_at: perfNow(performance),
  past: BoundedArray.create(history_limit),
  future: BoundedArray.create(history_limit),
}
let state = t => t.state
let canUndo = t => t.past->BoundedArray.isEmpty->Bool.not
let canRedo = t => t.future->BoundedArray.isEmpty->Bool.not

let step = (t, newState) => {
  let now = perfNow(performance)
  let last_step = t.stepped_at
  let state = t.state
  let should_coalesce = Js.Math.abs_float(now -. last_step) < coalesce_limit
  if should_coalesce {
    {
      state: newState,
      stepped_at: now,
      past: t.past,
      future: BoundedArray.create(history_limit),
    }
  } else {
    {
      state: newState,
      stepped_at: now,
      past: t.past->BoundedArray.push((last_step, state)),
      future: BoundedArray.create(history_limit),
    }
  }
}

let undo = t => {
  switch t.past->BoundedArray.pop {
  | Some(((time, state), newPast)) => {
      state: state,
      stepped_at: time,
      past: newPast,
      future: t.future->BoundedArray.push((t.stepped_at, t.state)),
    }
  | None => t
  }
}

let redo = t => {
  switch t.future->BoundedArray.pop {
  | Some(((time, state), newFuture)) => {
      state: state,
      stepped_at: time,
      past: t.past->BoundedArray.push((t.stepped_at, t.state)),
      future: newFuture,
    }
  | None => t
  }
}
