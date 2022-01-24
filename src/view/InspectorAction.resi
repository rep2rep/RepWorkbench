type t = {
  nodeId: Uuid.t,
  event: InspectorEvent.t,
}

let dispatch: (InspectorState.t, t) => (Uuid.t, option<InspectorState.Schema.t>)
