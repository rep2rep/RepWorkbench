module File = {
  type t =
    | NewModel(Uuid.t)
    | DeleteModel(Uuid.t)
    | FocusModel(Uuid.t)
    | DuplicateModel(Uuid.t, Uuid.t)
    | ImportModel(State.Model.t)
    | ReorderModels(array<Uuid.t>)

  let dispatch = (state, t) =>
    switch t {
    | NewModel(id) => state->State.createModel(id)
    | DeleteModel(id) => state->State.deleteModel(id)
    | FocusModel(id) => state->State.focusModel(id)
    | DuplicateModel(existing, new_) => state->State.duplicateModel(~existing, ~new_)
    | ImportModel(model) => state->State.importModel(model)
    | ReorderModels(order) => state->State.reorderModels(order)
    }
}

module Slots = {
  module Representation = {
    type t =
      | Domain(string)
      | Display(string)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Domain(s) => {...state, InspectorState.Representation.domain: s}
      | Display(s) => {...state, InspectorState.Representation.display: s}
      | Notes(s) => {...state, InspectorState.Representation.notes: s}
      }
  }

  module Scheme = {
    type t =
      | Concept_structure(string)
      | Graphic_structure(string)
      | Function(Function.t)
      | Explicit(bool)
      | Scope(Scope.t)
      | Organisation(string)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Concept_structure(s) => {...state, InspectorState.Scheme.concept_structure: s}
      | Graphic_structure(s) => {...state, InspectorState.Scheme.graphic_structure: s}
      | Function(s) => {...state, InspectorState.Scheme.function: s}
      | Explicit(s) => {...state, InspectorState.Scheme.explicit: s}
      | Scope(s) => {...state, InspectorState.Scheme.scope: s}
      | Organisation(s) => {...state, InspectorState.Scheme.organisation: s}
      | Notes(s) => {...state, InspectorState.Scheme.notes: s}
      }
  }

  module Dimension = {
    type t =
      | Concept(string)
      | Concept_scale(Quantity_scale.t)
      | Concept_attributes(list<Concept_attribute.t>)
      | Graphic(string)
      | Graphic_scale(Quantity_scale.t)
      | Graphic_attributes(list<Graphic_attribute.t>)
      | Function(Function.t)
      | Scope(Scope.t)
      | Explicit(bool)
      | Organisation(string)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Concept(s) => {...state, InspectorState.Dimension.concept: s}
      | Concept_scale(s) => {...state, InspectorState.Dimension.concept_scale: s}
      | Concept_attributes(s) => {...state, InspectorState.Dimension.concept_attributes: s}
      | Graphic(s) => {...state, InspectorState.Dimension.graphic: s}
      | Graphic_scale(s) => {...state, InspectorState.Dimension.graphic_scale: s}
      | Graphic_attributes(s) => {...state, InspectorState.Dimension.graphic_attributes: s}
      | Function(s) => {...state, InspectorState.Dimension.function: s}
      | Scope(s) => {...state, InspectorState.Dimension.scope: s}
      | Explicit(s) => {...state, InspectorState.Dimension.explicit: s}
      | Organisation(s) => {...state, InspectorState.Dimension.organisation: s}
      | Notes(s) => {...state, InspectorState.Dimension.notes: s}
      }
  }

  module Token = {
    type t =
      | Concept(string)
      | Graphic(string)
      | Is_class(bool)
      | Function(Function.t)
      | Explicit(bool)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Concept(s) => {...state, InspectorState.Token.concept: s}
      | Graphic(s) => {...state, InspectorState.Token.graphic: s}
      | Is_class(s) => {...state, InspectorState.Token.is_class: s}
      | Function(s) => {...state, InspectorState.Token.function: s}
      | Explicit(s) => {...state, InspectorState.Token.explicit: s}
      | Notes(s) => {...state, InspectorState.Token.notes: s}
      }
  }

  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)

  let dispatch = (state, t) =>
    switch (state, t) {
    | (InspectorState.Schema.Representation(state), Representation(e)) =>
      Representation.dispatch(state, e)->InspectorState.Schema.Representation
    | (InspectorState.Schema.Scheme(state), Scheme(e)) =>
      Scheme.dispatch(state, e)->InspectorState.Schema.Scheme
    | (InspectorState.Schema.Dimension(state), Dimension(e)) =>
      Dimension.dispatch(state, e)->InspectorState.Schema.Dimension
    | (InspectorState.Schema.Token(state), Token(e)) =>
      Token.dispatch(state, e)->InspectorState.Schema.Token
    | _ => state
    }
}

module Graph = {
  module Node = {
    type t =
      | UpdateName(string)
      | UpdateNameSuffix(option<string>)
      | UpdateReference(string)
      | UpdateReferenceSuffix(option<string>)
      | UpdateDashed(bool)

    let dispatch = (node, t) =>
      switch t {
      | UpdateName(name) => node->ModelNode.updatePayload(payload => {...payload, name: name})
      | UpdateNameSuffix(name_suffix) =>
        node->ModelNode.updatePayload(payload => {...payload, name_suffix: name_suffix})
      | UpdateReference(reference) =>
        node->ModelNode.updatePayload(payload => {...payload, reference: reference})
      | UpdateReferenceSuffix(reference_suffix) =>
        node->ModelNode.updatePayload(payload => {...payload, reference_suffix: reference_suffix})
      | UpdateDashed(dashed) =>
        node->ModelNode.updatePayload(payload => {...payload, dashed: dashed})
      }
  }

  type t =
    | AddNode(ModelNode.t)
    | UpdateNode(Uuid.t, Node.t)
    | DeleteNode(Uuid.t)
    | DuplicateNodes(Uuid.Map.t<Uuid.t>)
    | MoveNode(Uuid.t, float, float)
    | LinkNodes(Uuid.t, Uuid.t, ModelLink.Kind.t)
    | UnlinkNodes(Uuid.t, Uuid.t)
    | SetSelection(ModelSelection.t)

  let dispatch = (state, t) =>
    switch t {
    | AddNode(node) => state->ModelState.addNode(node)
    | UpdateNode(id, e) =>
      state->ModelState.updateNodes(node =>
        if ModelNode.id(node) === id {
          Node.dispatch(node, e)
        } else {
          node
        }
      )
    | DeleteNode(id) => state->ModelState.removeNode(id)
    | DuplicateNodes(idMap) => state->ModelState.duplicateNodes(idMap)
    | MoveNode(id, x, y) => state->ModelState.moveNode(id, ~x, ~y)
    | LinkNodes(source, target, kind) => {
        let modelSource = state->ModelState.nodeWithId(source)
        let modelTarget = state->ModelState.nodeWithId(target)
        switch (modelSource, modelTarget) {
        | (Some(source), Some(target)) =>
          state->ModelState.addLink(ModelLink.create(~source, ~target, kind))
        | _ => state
        }
      }
    | UnlinkNodes(source, target) => {
        let toRemove =
          state
          ->ModelState.graph
          ->ModelGraph.links
          ->Array.filter(link =>
            ModelLink.source(link) == source && ModelLink.target(link) == target
          )
        ModelState.removeLinks(state, toRemove)
      }
    | SetSelection(selection) => state->ModelState.setSelection(selection)
    }
}

module Model = {
  type t =
    | Rename(string)
    | SetNotes(string)
    | CreateNode(Uuid.t, float, float, ModelNode.Kind.t)
    | DeleteNode(Uuid.t)
    | DuplicateNodes(Uuid.Map.t<Uuid.t>)
    | Graph(Graph.t)
    | Slots(Uuid.t, Slots.t)

  let dispatch = (state, t) =>
    switch t {
    | Rename(name) =>
      state->State.Model.updateInfo(
        state->State.Model.info->(i => {...i, InspectorState.Model.name: name}),
      )
    | SetNotes(notes) =>
      state->State.Model.updateInfo(
        state->State.Model.info->(i => {...i, InspectorState.Model.notes: notes}),
      )
    | CreateNode(id, x, y, kind) => {
        let slots = InspectorState.Schema.empty(kind)
        let name = InspectorState.Schema.name(slots)
        let reference = InspectorState.Schema.reference(slots)
        let node = ModelNode.create(~name, ~reference, ~x, ~y, kind, id)
        let graph = state->State.Model.graph->Graph.dispatch(Graph.AddNode(node))
        let allSlots = state->State.Model.slots->Uuid.Map.set(id, slots)
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | DeleteNode(id) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.DeleteNode(id))
        let allSlots = state->State.Model.slots->Uuid.Map.remove(id)
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | DuplicateNodes(idMap) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.DuplicateNodes(idMap))
        let allSlots =
          idMap
          ->Uuid.Map.toArray
          ->Array.reduce(state->State.Model.slots, (slots, (oldId, newId)) => {
            slots
            ->Uuid.Map.get(oldId)
            ->Option.map(s => slots->Uuid.Map.set(newId, s))
            ->Option.getWithDefault(slots)
          })
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | Graph(ev) => {
        let graph = state->State.Model.graph->Graph.dispatch(ev)
        state->State.Model.updateGraph(graph)
      }
    | Slots(id, ev) => {
        let slots = state->State.Model.slots
        let s = slots->Uuid.Map.get(id)
        let s' = s->Option.map(Slots.dispatch(_, ev))
        let slots' = s'->Option.map(s' => slots->Uuid.Map.set(id, s'))->Option.getWithDefault(slots)
        state->State.Model.updateSlots(slots')
      }
    }

  let graphEvent = t =>
    switch t {
    | Rename(_) | SetNotes(_) | CreateNode(_, _, _, _) | DeleteNode(_) | DuplicateNodes(_) => None
    | Graph(e) => Some(e)
    | Slots(id, e) => {
        let e' = switch e {
        | Slots.Representation(Slots.Representation.Domain(s))
        | Slots.Scheme(Slots.Scheme.Concept_structure(s))
        | Slots.Dimension(Slots.Dimension.Concept(s))
        | Slots.Token(Slots.Token.Concept(s)) =>
          Some(Graph.Node.UpdateName(s))
        | Slots.Representation(Slots.Representation.Display(s))
        | Slots.Scheme(Slots.Scheme.Graphic_structure(s))
        | Slots.Dimension(Slots.Dimension.Graphic(s))
        | Slots.Token(Slots.Token.Graphic(s)) =>
          Some(Graph.Node.UpdateReference(s))
        | Slots.Dimension(Slots.Dimension.Concept_scale(s)) =>
          Some(
            Graph.Node.UpdateNameSuffix(
              Some(s->Quantity_scale.toString->String.slice(~from=0, ~to_=1)),
            ),
          )
        | Slots.Dimension(Slots.Dimension.Graphic_scale(s)) =>
          Some(
            Graph.Node.UpdateReferenceSuffix(
              Some(s->Quantity_scale.toString->String.slice(~from=0, ~to_=1)),
            ),
          )
        | Slots.Token(Slots.Token.Is_class(b)) => Some(Graph.Node.UpdateDashed(b))
        | _ => None
        }
        e'->Option.map(e' => Graph.UpdateNode(id, e'))
      }
    }
}

type rec t =
  | Model(Uuid.t, Model.t)
  | File(File.t)
  | Seq(array<t>)

let rec dispatch = (state, t) =>
  switch t {
  | Model(id, ev) =>
    state
    ->State.model(id)
    ->Option.map(model => state->State.updateModel(id, Model.dispatch(model, ev)))
    ->Option.getWithDefault(state)
  | File(ev) => File.dispatch(state, ev)
  | Seq(ts) => ts->Array.reduce(state, (state, ev) => dispatch(state, ev))
  }
