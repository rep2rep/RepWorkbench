type t =
  | NewModel(Uuid.t)
  | DeleteModel(Uuid.t)
  | FocusModel(Uuid.t)
  | DuplicateModel(Uuid.t, Uuid.t)
  | RenameModel(Uuid.t, string)
  | CreateNode(ModelNode.Kind.t, Uuid.t)
  | DeleteNode(Uuid.t)

let dispatch: (State.t, t) => State.t
