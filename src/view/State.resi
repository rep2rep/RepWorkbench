module Model: {
  type t

  let info: t => InspectorState.Model.t
  let graph: t => ModelState.t
  let slots: t => Uuid.Map.t<InspectorState.Schema.t>
  let slotsForSelection: (t, ModelSelection.t) => Uuid.Map.t<InspectorState.Schema.t>

  let updateInfo: (t, InspectorState.Model.t) => t
  let updateGraph: (t, ModelState.t) => t
  let updateSlots: (t, Uuid.Map.t<InspectorState.Schema.t>) => t

  module Stable: {
    module V1: {
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }

    module V2: {
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

let models: t => array<(Uuid.t, Model.t)>
let focused: t => option<Uuid.t>
let model: (t, Uuid.t) => option<Model.t>

let createModel: (t, Uuid.t) => t
let deleteModel: (t, Uuid.t) => t
let focusModel: (t, option<Uuid.t>) => t
let duplicateModel: (t, ~existing: Uuid.t, ~new_: Uuid.t) => t
let importModel: (t, Model.t) => t
let reorderModels: (t, array<Uuid.t>) => t

let updateModel: (t, Uuid.t, Model.t) => t
let undo: (t, Uuid.t) => t
let redo: (t, Uuid.t) => t
let canUndo: (t, Uuid.t) => bool
let canRedo: (t, Uuid.t) => bool
