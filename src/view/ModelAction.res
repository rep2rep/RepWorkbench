type t =
  | Create(float, float, ModelNode.Kind.t, Uuid.t)
  | Delete(Uuid.t)
  | Move(Uuid.t, float, float)
  | Update(Uuid.t, InspectorEvent.t)
  | Connect(Uuid.t, Uuid.t)
  | Anchor(Uuid.t, Uuid.t)
  | Relate(Uuid.t, Uuid.t)
  | Unlink(Uuid.t, Uuid.t)
  | Selection(ReactD3Graph.Graph.Selection.t)

let createNewNode = (state, x, y, kind, id) => {
  let (name, reference) = switch kind {
  | ModelNode.Kind.Representation => ("Representation", "Reference")
  | ModelNode.Kind.Scheme => ("Scheme", "Reference")
  | ModelNode.Kind.Dimension => ("Dimension", "Reference")
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

let updateNode = (state, nodeId, event) =>
  state->ModelState.updateNodes(node =>
    if ModelNode.id(node) == nodeId {
      let (name, reference) = switch (ModelNode.kind(node), event) {
      | (ModelNode.Kind.Representation, InspectorEvent.Representation(e)) =>
        switch e {
        | InspectorEvent.Representation.Domain(s) => (Some(s), None)
        | InspectorEvent.Representation.Display(s) => (None, Some(s))
        | _ => (None, None)
        }
      | (ModelNode.Kind.Scheme, InspectorEvent.Scheme(e)) =>
        switch e {
        | InspectorEvent.Scheme.Concept_structure(s) => (Some(s), None)
        | InspectorEvent.Scheme.Graphic_structure(s) => (None, Some(s))
        | _ => (None, None)
        }
      | (ModelNode.Kind.Dimension, InspectorEvent.Dimension(e)) =>
        switch e {
        | InspectorEvent.Dimension.Concept(s) => (Some(s), None)
        | InspectorEvent.Dimension.Graphic(s) => (None, Some(s))
        | _ => (None, None)
        }
      | (ModelNode.Kind.Token, InspectorEvent.Token(e)) =>
        switch e {
        | InspectorEvent.Token.Concept(s) => (Some(s), None)
        | InspectorEvent.Token.Graphic(s) => (None, Some(s))
        | _ => (None, None)
        }
      | _ => (None, None)
      }
      node->ModelNode.updatePayload(payload => {
        let name = name->Option.getWithDefault(payload.name)
        let reference = reference->Option.getWithDefault(payload.reference)
        switch (ModelNode.kind(node), event) {
        | (ModelNode.Kind.Token, InspectorEvent.Token(InspectorEvent.Token.Is_class(is_class))) => {
            ...payload,
            name: name,
            reference: reference,
            dashed: is_class,
          }
        | (
            ModelNode.Kind.Dimension,
            InspectorEvent.Dimension(InspectorEvent.Dimension.Concept_scale(q)),
          ) => {
            ...payload,
            name_suffix: Some(Quantity_scale.toString(q)->String.substring(~from=0, ~to_=1)),
          }
        | (
            ModelNode.Kind.Dimension,
            InspectorEvent.Dimension(InspectorEvent.Dimension.Graphic_scale(q)),
          ) => {
            ...payload,
            reference_suffix: Some(Quantity_scale.toString(q)->String.substring(~from=0, ~to_=1)),
          }

        | _ => {...payload, name: name, reference: reference}
        }
      })
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

let unlink = (state, source, target) => {
  let toRemove =
    state
    ->ModelState.graph
    ->ModelGraph.links
    ->Array.filter(link => ModelLink.source(link) == source && ModelLink.target(link) == target)
  ModelState.removeLinks(state, toRemove)
}

let setSelection = ModelState.setSelection

let dispatch = (state, action) =>
  switch action {
  | Create(x, y, kind, id) => createNewNode(state, x, y, kind, id)
  | Delete(id) => deleteNode(state, id)
  | Move(id, x, y) => moveNode(state, id, x, y)
  | Update(id, event) => updateNode(state, id, event)
  | Connect(source, target) => connect(state, source, target, ModelLink.Kind.Hierarchy)
  | Anchor(source, target) => connect(state, source, target, ModelLink.Kind.Anchor)
  | Relate(source, target) => connect(state, source, target, ModelLink.Kind.Relation)
  | Unlink(source, target) => unlink(state, source, target)
  | Selection(selection) => setSelection(state, selection)
  }
