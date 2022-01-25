type t =
  | NewModel(Uuid.t)
  | DeleteModel(Uuid.t)
  | FocusModel(Uuid.t)
  | RenameModel(Uuid.t, string)
  | CreateNode(ModelNode.Kind.t, Uuid.t)
  | DeleteNode(Uuid.t)

let createModel = State.createModel
let deleteModel = State.deleteModel
let focusModel = State.focusModel
let renameModel = State.renameModel

let createNode = (state, kind, id) => {
  let slots = switch kind {
  | ModelNode.Kind.Representation =>
    InspectorState.Schema.Representation(InspectorState.Representation.empty)
  | ModelNode.Kind.Scheme => InspectorState.Schema.Scheme(InspectorState.Scheme.empty)
  | ModelNode.Kind.Dimension => InspectorState.Schema.Dimension(InspectorState.Dimension.empty)
  | ModelNode.Kind.Token => InspectorState.Schema.Token(InspectorState.Token.empty)
  }
  state->State.updateSlots(id, Some(slots))
}
let deleteNode = (state, id) => state->State.updateSlots(id, None)

let dispatch = (state, action) =>
  switch action {
  | NewModel(id) => createModel(state, id)
  | DeleteModel(id) => deleteModel(state, id)
  | FocusModel(id) => focusModel(state, id)
  | RenameModel(id, name) => renameModel(state, id, name)
  | CreateNode(kind, id) => createNode(state, kind, id)
  | DeleteNode(id) => deleteNode(state, id)
  }
