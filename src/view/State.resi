module Model: {
  type t

  let info: t => InspectorState.Model.t
  let graph: t => ModelState.t
  let slots: t => Gid.Map.t<InspectorState.Schema.t>
  let slotsForSelection: (t, ModelSelection.t) => Gid.Map.t<InspectorState.Schema.t>

  let updateInfo: (t, InspectorState.Model.t) => t
  let updateGraph: (t, ModelState.t) => t
  let updateSlots: (t, Gid.Map.t<InspectorState.Schema.t>) => t

  module Stable: {
    module V1: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }

    module V2: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }

    module V3: {
      type t = t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }
  }
}

type t

let store: t => unit
let load: unit => option<t>

let empty: t

let models: t => array<(Gid.t, Model.t)>
let focused: t => option<Gid.t>
let model: (t, Gid.t) => option<Model.t>

let createModel: (t, Gid.t) => t
let deleteModel: (t, Gid.t) => t
let focusModel: (t, option<Gid.t>) => t
let duplicateModel: (t, ~existing: Gid.t, ~new_: Gid.t) => t
let importModel: (t, Model.t) => t
let reorderModels: (t, array<Gid.t>) => t

let updateModel: (t, Gid.t, Model.t) => t
let undo: (t, Gid.t) => t
let redo: (t, Gid.t) => t
let canUndo: (t, Gid.t) => bool
let canRedo: (t, Gid.t) => bool
