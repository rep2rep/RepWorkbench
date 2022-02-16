module Model: {
  type t =
    | CreateNode(ModelNode.Kind.t, Uuid.t)
    | DeleteNode(Uuid.t)

  let dispatch: (State.Model.t, t) => State.Model.t
}

type t =
  | NewModel(Uuid.t)
  | DeleteModel(Uuid.t)
  | FocusModel(Uuid.t)
  | DuplicateModel(Uuid.t, Uuid.t)
  | ImportModel(State.Model.t)
  | ReorderModels(array<Uuid.t>)

let dispatch: (State.t, t) => State.t
