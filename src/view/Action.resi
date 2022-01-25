type t =
  | NewModel(Uuid.t)
  | DeleteModel(Uuid.t)
  | FocusModel(Uuid.t)
  | CreateNode(ModelNode.Kind.t, Uuid.t)
  | DeleteNode(Uuid.t)

let dispatch: (State.t, t) => State.t
