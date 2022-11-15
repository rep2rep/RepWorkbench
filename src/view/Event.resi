module Intelligence: {
  type t =
    | Init
    | Response(Intelligence_Intf.Response.t)
    | Focus(Gid.t, option<Gid.t>) // Model, Intelligence
}

module File: {
  type t =
    | NewModel(Gid.t, FileTree.Path.t)
    | NewFolder(Gid.t, FileTree.Path.t)
    | DeleteModel(Gid.t)
    | DeleteFolder(Gid.t)
    | FocusModel(option<Gid.t>)
    | DuplicateModel(Gid.t, Gid.t)
    | ImportModel(Gid.t, State.Model.t, FileTree.Path.t)
    | ReorderModels(FileTree.t<Gid.t>)
    | RenameFolder(Gid.t, string)
    | Undo(Gid.t)
    | Redo(Gid.t)
    | ViewTransform(Gid.t, ReactD3Graph.Graph.ViewTransform.t)
    | Intelligence(Intelligence.t)
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
      | Is_class(option<bool>)
      | Function(option<Function.t>)
      | Explicit(option<bool>)
      | Notes(string)
  }

  module Placeholder: {
    type t =
      | Description(string)
      | IsIntensional(option<bool>)
      | Notes(string)
  }

  module Hierarchy: {
    type t =
      | Notes(string)
      | Order(option<int>)
  }

  module Anchor: {
    type t =
      | Notes(string)
      | Order(option<int>)
  }

  module Generic: {
    type t = Notes(string)
  }

  type t =
    | Representation(Representation.t)
    | Scheme(Scheme.t)
    | Dimension(Dimension.t)
    | Token(Token.t)
    | Placeholder(Placeholder.t)
    | Hierarchy(Hierarchy.t)
    | Anchor(Anchor.t)
    | Generic(Generic.t)
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

  module Link: {
    type t = UpdateOrder(option<int>)
  }

  type rec t =
    | AddNode(ModelNode.t)
    | UpdateNode(Gid.t, Node.t)
    | UpdateLink(Gid.t, Link.t)
    | DeleteNode(Gid.t)
    | Duplicate(Gid.Map.t<Gid.t>)
    | MoveNode(Gid.t, float, float)
    | LinkNodes({
        linkId: Gid.t,
        source: Gid.t,
        target: Gid.t,
        kind: ModelLink.Kind.t,
        label: option<int>,
      })
    | DeleteLink(Gid.t)
    | SetSelection(ModelSelection.t)
    | Seq(array<t>)
}

module Model: {
  type rec t =
    | Rename(string)
    | SetNotes(string)
    | SetMetrics(option<ModelMetrics.t>, float)
    | CreateNode(Gid.t, float, float, ModelNode.Kind.t)
    | DeleteNode(Gid.t)
    | Duplicate(Gid.Map.t<Gid.t>)
    | LinkNodes({
        linkId: Gid.t,
        source: Gid.t,
        target: Gid.t,
        kind: ModelLink.Kind.t,
        label: option<int>,
      })
    | DeleteLink(Gid.t)
    | Graph(Graph.t)
    | Slots(Gid.t, Slots.t)
    | Seq(array<t>)

  // Take what is potentially an event for another "direction"
  // And convert it to a graph event. Mainly used to redirect "slots" events.
  let graphEvent: t => option<Graph.t>
}

type t =
  | Model(Gid.t, Model.t)
  | File(File.t)

let dispatch: (State.t, t, ~atTime: float) => State.t
let shouldTriggerIntelligence: t => bool

// NOT STABLE
// These JSON converters are not stable, so not suitable for long-term storage.
// These are only used for recordings, which are internal to the RepSysLab; we
// can manage the versioning here quite tightly, and build a Recording Player
// for the specific version from the git repo.
let toJson: t => Js.Json.t
let fromJson: Js.Json.t => Or_error.t<t>
