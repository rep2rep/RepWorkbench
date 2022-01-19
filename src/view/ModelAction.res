type nodeId = ReactD3Graph.Node.Id.t

type t =
  | Create(float, float, ModelNode.Kind.t)
  | Connect(nodeId, nodeId)
  | Anchor(nodeId, nodeId)
  | Relate(nodeId, nodeId)
  | Selection(ReactD3Graph.Graph.Selection.t)

let createNewNode = (state, x, y, kind) => {
  let (name, reference) = switch kind {
  | ModelNode.Kind.Representation => ("Representation", "Reference")
  | ModelNode.Kind.Scheme => ("Scheme", "Reference")
  | ModelNode.Kind.Dimension => ("Dimension, Q", "Reference, Q")
  | ModelNode.Kind.Token => ("Token", "Reference")
  }
  let node = ModelNode.create(~name, ~reference, ~x, ~y, kind)
  ModelState.addNode(state, node)
}

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
  | Create(x, y, kind) => createNewNode(state, x, y, kind)
  | Connect(source, target) => connect(state, source, target, ModelLink.Kind.Heirarchy)
  | Anchor(source, target) => connect(state, source, target, ModelLink.Kind.Anchor)
  | Relate(source, target) => connect(state, source, target, ModelLink.Kind.Relation)
  | Selection(selection) => setSelection(state, selection)
  }
