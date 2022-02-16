type t =
  | UpdateSchema(Uuid.t, InspectorEvent.t)
  | DuplicateSchema(Uuid.Map.t<Uuid.t>)
  | UpdateGlobal(InspectorEvent.t)

let dispatch: (InspectorState.t, t) => InspectorState.t
