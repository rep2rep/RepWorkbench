type t =
  | NewModel(Uuid.t)
  | DeleteModel(Uuid.t)
  | FocusModel(Uuid.t)
  | RenameModel(Uuid.t, string)
  | ReorderModels(array<State.Model.t>)
  | CreateNode(ModelNode.Kind.t, Uuid.t)
  | DeleteNode(Uuid.t)

let dispatch: (State.t, t) => State.t
