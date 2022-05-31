module Model: {
  type t

  let info: t => InspectorState.Model.t
  let graph: t => ModelState.t
  let slots: t => Gid.Map.t<InspectorState.SchemaOrLink.t>
  let slotsForSelection: (t, ModelSelection.t) => array<(Gid.t, InspectorState.SchemaOrLink.t)>

  let updateInfo: (t, InspectorState.Model.t) => t
  let updateGraph: (t, ModelState.t) => t
  let updateSlots: (t, Gid.Map.t<InspectorState.SchemaOrLink.t>) => t

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
      type t
      let toJson: t => Js.Json.t
      let fromJson: Js.Json.t => Or_error.t<t>
    }

    module V4: {
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
let dagstuhl: t

let models: t => array<(Gid.t, Model.t)>
let focused: t => option<Gid.t>
let model: (t, Gid.t) => option<Model.t>
let latestIntelligence: t => option<Intelligence_Intf.Response.t>
let lastRequestedIntelligence: t => option<Gid.t>
let intelligenceIsUpToDate: t => bool
let focusedErrorOrWarning: t => option<Gid.t>

let createModel: (t, Gid.t) => t
let deleteModel: (t, Gid.t) => t
let focusModel: (t, option<Gid.t>) => t
let duplicateModel: (t, ~existing: Gid.t, ~new_: Gid.t) => t
let importModel: (t, Model.t) => t
let reorderModels: (t, array<Gid.t>) => t
let setLatestIntelligence: (t, option<Intelligence_Intf.Response.t>) => t
let setLastRequestedIntelligence: (t, option<Gid.t>) => t
let focusErrorOrWarning: (t, option<Gid.t>) => t

let updateModel: (t, Gid.t, Model.t) => t
let undo: (t, Gid.t) => t
let redo: (t, Gid.t) => t
let canUndo: (t, Gid.t) => bool
let canRedo: (t, Gid.t) => bool
