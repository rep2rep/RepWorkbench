module Model: {
  type t

  let id: t => Uuid.t
  let name: t => string

  module Stable: {
    module V1: {
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

let models: t => array<Model.t>
let model: (t, Uuid.t) => option<Model.t>
let focusedName: t => option<string>
let focusedId: t => option<Uuid.t>

let modelState: t => ModelState.t
let inspectorState: t => InspectorState.t

let createModel: (t, Uuid.t) => t
let deleteModel: (t, Uuid.t) => t
let focusModel: (t, Uuid.t) => t
let duplicateModel: (t, ~existing: Uuid.t, ~new_: Uuid.t) => t
let importModel: (t, Model.t) => t
let renameModel: (t, Uuid.t, string) => t
let reorderModels: (t, array<Model.t>) => t

let updateModel: (t, ModelState.t) => t
let updateSlots: (t, Uuid.t, option<InspectorState.Schema.t>) => t
