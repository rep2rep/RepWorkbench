type t =
  | Update(Uuid.t, InspectorEvent.t)
  | Duplicate(Uuid.Map.t<Uuid.t>)

let dispatch: (InspectorState.t, t) => array<(Uuid.t, option<InspectorState.Schema.t>)>
