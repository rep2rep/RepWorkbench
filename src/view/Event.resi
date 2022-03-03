module File: {
  type t =
    | NewModel(Uuid.t)
    | DeleteModel(Uuid.t)
    | FocusModel(option<Uuid.t>)
    | DuplicateModel(Uuid.t, Uuid.t)
    | ImportModel(State.Model.t)
    | ReorderModels(array<Uuid.t>)
    | Undo(Uuid.t)
    | Redo(Uuid.t)
}

module Slots: {
  module Representation: {
    type t =
      | Domain(string)
      | Display(string)
      | Notes(string)
  }

  module Scheme: {
    type t =
      | Concept_structure(string)
      | Graphic_structure(string)
      | Function(option<Function.t>)
      | Explicit(option<bool>)
      | Scope(option<Scope.t>)
      | Organisation(string)
      | Notes(string)
  }

  module Dimension: {
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
  }

  module Token: {
    type t =
      | Concept(string)
      | Graphic(string)
      | Is_class(bool)
      | Function(Function.t)
      | Explicit(bool)
      | Notes(string)
  }

  module Placeholder: {
    type t =
      | Description(string)
      | IsIntensional(bool)
      | Notes(string)
  }

  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)
    | Placeholder(Placeholder.t)
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

  type rec t =
    | AddNode(ModelNode.t)
    | UpdateNode(Uuid.t, Node.t)
    | DeleteNode(Uuid.t)
    | DuplicateNodes(Uuid.Map.t<Uuid.t>)
    | MoveNode(Uuid.t, float, float)
    | LinkNodes(Uuid.t, Uuid.t, ModelLink.Kind.t)
    | UnlinkNodes(Uuid.t, Uuid.t)
    | SetSelection(ModelSelection.t)
    | Seq(array<t>)
}

module Model: {
  type rec t =
    | Rename(string)
    | SetNotes(string)
    | CreateNode(Uuid.t, float, float, ModelNode.Kind.t)
    | DeleteNode(Uuid.t)
    | DuplicateNodes(Uuid.Map.t<Uuid.t>)
    | Graph(Graph.t)
    | Slots(Uuid.t, Slots.t)
    | Seq(array<t>)

  // Take what is potentially an event for another "direction"
  // And convert it to a graph event. Mainly used to redirect "slots" events.
  let graphEvent: t => option<Graph.t>
}

type t =
  | Model(Uuid.t, Model.t)
  | File(File.t)

let dispatch: (State.t, t) => State.t
