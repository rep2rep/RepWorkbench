type t =
  | CreateNode(ModelNode.Kind.t, Uuid.t)
  | DeleteNode(Uuid.t)

let create = (state, kind, id) => {
  let slots = switch kind {
  | ModelNode.Kind.Representation =>
    InspectorState.Representation(InspectorState.Representation.empty)
  | ModelNode.Kind.Scheme => InspectorState.Scheme(InspectorState.Scheme.empty)
  | ModelNode.Kind.Dimension => InspectorState.Dimension(InspectorState.Dimension.empty)
  | ModelNode.Kind.Token => InspectorState.Token(InspectorState.Token.empty)
  }
  state->State.updateSlots(id, Some(slots))
}
let delete = (state, id) => state->State.updateSlots(id, None)

let dispatch = (state, action) =>
  switch action {
  | CreateNode(kind, id) => create(state, kind, id)
  | DeleteNode(id) => delete(state, id)
  }
