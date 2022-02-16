module File: {
  type t =
    | NewModel(Uuid.t)
    | DeleteModel(Uuid.t)
    | FocusModel(Uuid.t)
    | DuplicateModel(Uuid.t, Uuid.t)
    | ImportModel(State.Model.t)
    | ReorderModels(array<Uuid.t>)

  // let dispatch: (State.t, t) => State.t
}

module Slots: {
  module Representation: {
    type t =
      | Domain(string)
      | Display(string)
      | Notes(string)

    // let dispatch: (InspectorState.Representation.t, t) => InspectorState.Representation.t
  }

  module Scheme: {
    type t =
      | Concept_structure(string)
      | Graphic_structure(string)
      | Function(Function.t)
      | Explicit(bool)
      | Scope(Scope.t)
      | Organisation(string)
      | Notes(string)

    // let dispatch: (InspectorState.Scheme.t, t) => InspectorState.Scheme.t
  }

  module Dimension: {
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

    // let dispatch: (InspectorState.Dimension.t, t) => InspectorState.Dimension.t
  }

  module Token: {
    type t =
      | Concept(string)
      | Graphic(string)
      | Is_class(bool)
      | Function(Function.t)
      | Explicit(bool)
      | Notes(string)

    // let dispatch: (InspectorState.Token.t, t) => InspectorState.Token.t
  }

  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)

  let dispatch: (InspectorState.Schema.t, t) => InspectorState.Schema.t
}

module Graph: {
  module Node: {
    type t =
      | UpdateName(string)
      | UpdateNameSuffix(option<string>)
      | UpdateReference(string)
      | UpdateReferenceSuffix(option<string>)
      | UpdateDashed(bool)
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

  // let dispatch: (ModelState.t, t) => ModelState.t
}

module Model: {
  type t =
    | Rename(string)
    | SetNotes(string)
    | CreateNode(Uuid.t, float, float, ModelNode.Kind.t)
    | DeleteNode(Uuid.t)
    | DuplicateNodes(Uuid.Map.t<Uuid.t>)
    | Graph(Graph.t)
    | Slots(Uuid.t, Slots.t)

  // let dispatch: (State.Model.t, t) => State.Model.t
  // Take what is potentially an event for another "direction"
  // And convert it to a graph event. Mainly used to redirect "slots" events.
  let graphEvent: t => option<Graph.t>
}

type t =
  | Model(Uuid.t, Model.t)
  | File(File.t)

let dispatch: (State.t, t) => State.t
