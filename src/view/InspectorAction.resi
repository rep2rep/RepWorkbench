type t = Update(Uuid.t, InspectorEvent.t)

let dispatch: (InspectorState.t, t) => array<(Uuid.t, option<InspectorState.Schema.t>)>
