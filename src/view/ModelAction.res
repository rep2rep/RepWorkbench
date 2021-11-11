type t = Create(float, float, ModelNode.Kind.t)

let createNewNode = (state, x, y, kind) => {
  let (name, reference) = switch kind {
  | ModelNode.Kind.Representation => ("Representation", "Reference")
  | ModelNode.Kind.Scheme => ("Scheme", "Reference")
  | ModelNode.Kind.Dimension => ("Dimension, Q", "Reference, Q")
  | ModelNode.Kind.Token => ("Token", "Reference")
  }
  let node = ModelNode.create(name, reference, x, y, kind)
  ModelState.addNode(state, node)
}

let dispatch = (state, action) =>
  switch action {
  | Create(x, y, kind) => createNewNode(state, x, y, kind)
  }
