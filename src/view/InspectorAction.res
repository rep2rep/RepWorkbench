type t = Update(Uuid.t, InspectorEvent.t)

let dispatch = (state, t) => {
  let Update(key, event) = t
  let newState = switch state {
  | InspectorState.Single(s) => Some(InspectorState.Schema.applyEvent(s, event))
  | _ => None
  }
  (key, newState)
}
