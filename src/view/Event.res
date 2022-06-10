module Intelligence = {
  type t =
    | Init
    | Response(Intelligence_Intf.Response.t)
    | Focus(option<Gid.t>)

  let dispatch = (state, t) =>
    switch t {
    | Init => state
    | Response(response) => state->State.setLatestIntelligence(Some(response))
    | Focus(id) => state->State.focusErrorOrWarning(id)
    }
}

module File = {
  type t =
    | NewModel(Gid.t)
    | DeleteModel(Gid.t)
    | FocusModel(option<Gid.t>)
    | DuplicateModel(Gid.t, Gid.t)
    | ImportModel(State.Model.t)
    | ReorderModels(array<Gid.t>)
    | Undo(Gid.t)
    | Redo(Gid.t)
    | Intelligence(Intelligence.t)

  let dispatch = (state, t) =>
    switch t {
    | NewModel(id) => state->State.createModel(id)
    | DeleteModel(id) => state->State.deleteModel(id)
    | FocusModel(id) => state->State.focusModel(id)
    | DuplicateModel(existing, new_) => state->State.duplicateModel(~existing, ~new_)
    | ImportModel(model) => state->State.importModel(model)
    | ReorderModels(order) => state->State.reorderModels(order)
    | Undo(id) => state->State.undo(id)
    | Redo(id) => state->State.redo(id)
    | Intelligence(i) => state->Intelligence.dispatch(i)
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
      | Function(option<Function.t>)
      | Explicit(option<bool>)
      | Scope(option<Scope.t>)
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
      | Concept_scale(option<Quantity_scale.t>)
      | Concept_attributes(list<string>)
      | Graphic(string)
      | Graphic_scale(option<Quantity_scale.t>)
      | Graphic_attributes(list<string>)
      | Function(option<Function.t>)
      | Scope(option<Scope.t>)
      | Explicit(option<bool>)
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
      | Is_class(option<bool>)
      | Function(option<Function.t>)
      | Explicit(option<bool>)
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

  module Placeholder = {
    type t =
      | Description(string)
      | IsIntensional(option<bool>)
      | Notes(string)

    let dispatch = (state, t) =>
      switch t {
      | Description(s) => {...state, InspectorState.Placeholder.description: s}
      | IsIntensional(s) => {...state, InspectorState.Placeholder.isIntensional: s}
      | Notes(s) => {...state, InspectorState.Placeholder.notes: s}
      }
  }

  module Hierarchy = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Hierarchy.notes: s}
      }
  }

  module Anchor = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Anchor.notes: s}
      }
  }

  module Relation = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Relation.notes: s}
      }
  }

  module Overlap = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Overlap.notes: s}
      }
  }

  module Disjoint = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Disjoint.notes: s}
      }
  }

  module Generic = {
    type t = Notes(string)

    let dispatch = (_, t) =>
      switch t {
      | Notes(s) => {InspectorState.Generic.notes: s}
      }
  }

  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)
    | Placeholder(Placeholder.t)
    | Hierarchy(Hierarchy.t)
    | Anchor(Anchor.t)
    | Relation(Relation.t)
    | Overlap(Overlap.t)
    | Disjoint(Disjoint.t)
    | Generic(Generic.t)

  let dispatch = (state, t) =>
    switch (state, t) {
    | (
        InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Representation(state)),
        Representation(e),
      ) =>
      Representation.dispatch(state, e)
      ->InspectorState.Schema.Representation
      ->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Scheme(state)), Scheme(e)) =>
      Scheme.dispatch(state, e)->InspectorState.Schema.Scheme->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Dimension(state)), Dimension(e)) =>
      Dimension.dispatch(state, e)
      ->InspectorState.Schema.Dimension
      ->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Token(state)), Token(e)) =>
      Token.dispatch(state, e)->InspectorState.Schema.Token->InspectorState.SchemaOrLink.Schema
    | (
        InspectorState.SchemaOrLink.Schema(InspectorState.Schema.Placeholder(state)),
        Placeholder(e),
      ) =>
      Placeholder.dispatch(state, e)
      ->InspectorState.Schema.Placeholder
      ->InspectorState.SchemaOrLink.Schema
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Hierarchy(state)), Hierarchy(e)) =>
      Hierarchy.dispatch(state, e)->InspectorState.Link.Hierarchy->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Anchor(state)), Anchor(e)) =>
      Anchor.dispatch(state, e)->InspectorState.Link.Anchor->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Relation(state)), Relation(e)) =>
      Relation.dispatch(state, e)->InspectorState.Link.Relation->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Overlap(state)), Overlap(e)) =>
      Overlap.dispatch(state, e)->InspectorState.Link.Overlap->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Disjoint(state)), Disjoint(e)) =>
      Disjoint.dispatch(state, e)->InspectorState.Link.Disjoint->InspectorState.SchemaOrLink.Link
    | (InspectorState.SchemaOrLink.Link(InspectorState.Link.Generic(state)), Generic(e)) =>
      Generic.dispatch(state, e)->InspectorState.Link.Generic->InspectorState.SchemaOrLink.Link

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

  type rec t =
    | AddNode(ModelNode.t)
    | UpdateNode(Gid.t, Node.t)
    | DeleteNode(Gid.t)
    | Duplicate(Gid.Map.t<Gid.t>)
    | MoveNode(Gid.t, float, float)
    | LinkNodes({linkId: Gid.t, source: Gid.t, target: Gid.t, kind: ModelLink.Kind.t})
    | DeleteLink(Gid.t)
    | SetSelection(ModelSelection.t)
    | Seq(array<t>)

  let rec dispatch = (state, t) =>
    switch t {
    | Seq(ts) => ts->Array.reduce(state, (state, t) => dispatch(state, t))
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
    | Duplicate(idMap) => state->ModelState.duplicateNodes(idMap)
    | MoveNode(id, x, y) => state->ModelState.moveNode(id, ~x, ~y)
    | LinkNodes({linkId, source, target, kind}) => {
        let modelSource = state->ModelState.nodeWithId(source)
        let modelTarget = state->ModelState.nodeWithId(target)
        switch (modelSource, modelTarget) {
        | (Some(source), Some(target)) =>
          state->ModelState.addLink(ModelLink.create(~linkId, ~source, ~target, kind))
        | _ => state
        }
      }
    | DeleteLink(linkId) => state->ModelState.removeLink(linkId)
    | SetSelection(selection) => state->ModelState.setSelection(selection)
    }
}

module Model = {
  type rec t =
    | Rename(string)
    | SetNotes(string)
    | CreateNode(Gid.t, float, float, ModelNode.Kind.t)
    | DeleteNode(Gid.t)
    | Duplicate(Gid.Map.t<Gid.t>)
    | LinkNodes({linkId: Gid.t, source: Gid.t, target: Gid.t, kind: ModelLink.Kind.t})
    | DeleteLink(Gid.t)
    | Graph(Graph.t)
    | Slots(Gid.t, Slots.t)
    | Seq(array<t>)

  let rec dispatch = (state, t) =>
    switch t {
    | Seq(ts) => ts->Array.reduce(state, (state, t) => dispatch(state, t))
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
        let allSlots =
          state->State.Model.slots->Gid.Map.set(id, InspectorState.SchemaOrLink.Schema(slots))
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | DeleteNode(id) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.DeleteNode(id))
        let allSlots = state->State.Model.slots->Gid.Map.remove(id)
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | Duplicate(idMap) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.Duplicate(idMap))
        let allSlots =
          idMap
          ->Gid.Map.toArray
          ->Array.reduce(state->State.Model.slots, (slots, (oldId, newId)) => {
            slots
            ->Gid.Map.get(oldId)
            ->Option.map(s => slots->Gid.Map.set(newId, InspectorState.SchemaOrLink.duplicate(s)))
            ->Option.getWithDefault(slots)
          })
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | LinkNodes({linkId, source, target, kind}) => {
        let slots = InspectorState.Link.empty(kind)->InspectorState.SchemaOrLink.Link
        let allSlots = state->State.Model.slots->Gid.Map.set(linkId, slots)
        let state = state->State.Model.updateSlots(allSlots)
        let graph =
          state
          ->State.Model.graph
          ->Graph.dispatch(
            Graph.LinkNodes({linkId: linkId, source: source, target: target, kind: kind}),
          )
        state->State.Model.updateGraph(graph)
      }
    | DeleteLink(linkId) => {
        let graph = state->State.Model.graph->Graph.dispatch(Graph.DeleteLink(linkId))
        let allSlots = state->State.Model.slots->Gid.Map.remove(linkId)
        state->State.Model.updateGraph(graph)->State.Model.updateSlots(allSlots)
      }
    | Graph(ev) => {
        let graph = state->State.Model.graph->Graph.dispatch(ev)
        state->State.Model.updateGraph(graph)
      }
    | Slots(id, ev) => {
        let slots = state->State.Model.slots
        let s = slots->Gid.Map.get(id)
        let s' = s->Option.map(Slots.dispatch(_, ev))
        let slots' = s'->Option.map(s' => slots->Gid.Map.set(id, s'))->Option.getWithDefault(slots)
        state->State.Model.updateSlots(slots')
      }
    }

  let rec graphEvent = t =>
    switch t {
    | Rename(_)
    | SetNotes(_)
    | CreateNode(_, _, _, _)
    | DeleteNode(_)
    | Duplicate(_)
    | DeleteLink(_)
    | LinkNodes(_) =>
      None
    | Seq(ts) => ts->Array.mapPartial(graphEvent)->Graph.Seq->Some
    | Graph(e) => Some(e)
    | Slots(id, e) => {
        let e' = switch e {
        | Slots.Representation(Slots.Representation.Domain(s))
        | Slots.Scheme(Slots.Scheme.Concept_structure(s))
        | Slots.Dimension(Slots.Dimension.Concept(s))
        | Slots.Token(Slots.Token.Concept(s))
        | Slots.Placeholder(Slots.Placeholder.Description(s)) =>
          Some(Graph.Node.UpdateName(s))
        | Slots.Representation(Slots.Representation.Display(s))
        | Slots.Scheme(Slots.Scheme.Graphic_structure(s))
        | Slots.Dimension(Slots.Dimension.Graphic(s))
        | Slots.Token(Slots.Token.Graphic(s)) =>
          Some(Graph.Node.UpdateReference(s))
        | Slots.Dimension(Slots.Dimension.Concept_scale(s)) =>
          Some(
            Graph.Node.UpdateNameSuffix(
              Some(
                s
                ->Option.map(Quantity_scale.toString)
                ->Option.getWithDefault("-")
                ->String.slice(~from=0, ~to_=1),
              ),
            ),
          )
        | Slots.Dimension(Slots.Dimension.Graphic_scale(s)) =>
          Some(
            Graph.Node.UpdateReferenceSuffix(
              Some(
                s
                ->Option.map(Quantity_scale.toString)
                ->Option.getWithDefault("-")
                ->String.slice(~from=0, ~to_=1),
              ),
            ),
          )
        | Slots.Token(Slots.Token.Is_class(b))
        | Slots.Placeholder(Slots.Placeholder.IsIntensional(b)) =>
          Some(Graph.Node.UpdateDashed(b->Option.getWithDefault(false)))
        | _ => None
        }
        e'->Option.map(e' => Graph.UpdateNode(id, e'))
      }
    }
}

type t =
  | Model(Gid.t, Model.t)
  | File(File.t)

let dispatch = (state, t) =>
  switch t {
  | Model(id, ev) => state->State.updateModel(id, model => Model.dispatch(model, ev))
  | File(ev) => File.dispatch(state, ev)
  }

let rec shouldTriggerIntelligence = e =>
  switch e {
  | File(File.Intelligence(Intelligence.Init)) => true // MUST be true
  | Model(_, Model.Graph(Graph.SetSelection(_)))
  | Model(_, Model.Graph(Graph.MoveNode(_, _, _)))
  | Model(_, Model.Rename(_))
  | Model(_, Model.SetNotes(_))
  | Model(_, Model.Slots(_, Slots.Representation(Slots.Representation.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Scheme(Slots.Scheme.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Dimension(Slots.Dimension.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Token(Slots.Token.Notes(_))))
  | Model(_, Model.Slots(_, Slots.Placeholder(Slots.Placeholder.Notes(_))))
  | File(File.Intelligence(_)) => false
  | Model(id, Model.Seq(vs)) =>
    vs->Array.map(v => Model(id, v))->Array.some(shouldTriggerIntelligence)
  | _ => true
  }
