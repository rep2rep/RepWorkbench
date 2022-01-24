type t = {
  nodeId: Uuid.t,
  event: InspectorEvent.t,
}

let dispatch = (state, t) => {
  let key = t.nodeId
  let newState = switch state {
  | InspectorState.Single(s) => Some(InspectorState.Schema.applyEvent(s, t.event))
  | _ => None
  }
  (key, newState)
}
