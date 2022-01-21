type t =
  | Create(float, float, ModelNode.Kind.t, Uuid.t)
  | Delete(Uuid.t)
  | Move(Uuid.t, float, float)
  | Connect(Uuid.t, Uuid.t)
  | Anchor(Uuid.t, Uuid.t)
  | Relate(Uuid.t, Uuid.t)
  | Selection(ReactD3Graph.Graph.Selection.t)

let createNewNode = (state, x, y, kind, id) => {
  let (name, reference) = switch kind {
  | ModelNode.Kind.Representation => ("Representation", "Reference")
  | ModelNode.Kind.Scheme => ("Scheme", "Reference")
  | ModelNode.Kind.Dimension => ("Dimension, Q", "Reference, Q")
  | ModelNode.Kind.Token => ("Token", "Reference")
  }
  let node = ModelNode.create(~name, ~reference, ~x, ~y, kind, id)
  ModelState.addNode(state, node)
}

let deleteNode = ModelState.removeNode
let moveNode = (state, nodeId, x, y) =>
  state->ModelState.updateNodes(node =>
    if ModelNode.id(node) == nodeId {
      node->ModelNode.setPosition(~x, ~y)
    } else {
      node
    }
  )

let connect = (state, source, target, kind) => {
  let modelSource = state->ModelState.nodeWithId(source)
  let modelTarget = state->ModelState.nodeWithId(target)
  switch (modelSource, modelTarget) {
  | (Some(source), Some(target)) => {
      let link = ModelLink.create(~source, ~target, kind)
      ModelState.addLink(state, link)
    }
  | _ => state
  }
}

let setSelection = ModelState.setSelection

let dispatch = (state, action) =>
  switch action {
  | Create(x, y, kind, id) => createNewNode(state, x, y, kind, id)
  | Delete(id) => deleteNode(state, id)
  | Move(id, x, y) => moveNode(state, id, x, y)
  | Connect(source, target) => connect(state, source, target, ModelLink.Kind.Hierarchy)
  | Anchor(source, target) => connect(state, source, target, ModelLink.Kind.Anchor)
  | Relate(source, target) => connect(state, source, target, ModelLink.Kind.Relation)
  | Selection(selection) => setSelection(state, selection)
  }
