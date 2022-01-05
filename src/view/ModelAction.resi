type nodeId = ReactD3Graph.Node.Id.t

type t =
  | Create(float, float, ModelNode.Kind.t)
  | Connect(nodeId, nodeId)
  | Anchor(nodeId, nodeId)
  | Relate(nodeId, nodeId)
  | Select(nodeId)
  | Deselect(nodeId)
  | ClearSelection

let dispatch: (ModelState.t, t) => ModelState.t
