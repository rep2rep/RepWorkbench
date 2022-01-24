type t =
  | Create(float, float, ModelNode.Kind.t, Uuid.t)
  | Delete(Uuid.t)
  | Move(Uuid.t, float, float)
  | Update(Uuid.t, InspectorEvent.t)
  | Connect(Uuid.t, Uuid.t)
  | Anchor(Uuid.t, Uuid.t)
  | Relate(Uuid.t, Uuid.t)
  | Selection(ReactD3Graph.Graph.Selection.t)

let dispatch: (ModelState.t, t) => ModelState.t
