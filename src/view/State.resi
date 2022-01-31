module Model: {
  type t

  let id: t => Uuid.t
  let name: t => string
}

type t

let store: t => unit
let load: unit => option<t>
let dump: t => Js.Json.t

let empty: t

let models: t => array<Model.t>
let focusedName: t => option<string>
let focusedId: t => option<Uuid.t>

let modelState: t => ModelState.t
let inspectorState: t => InspectorState.t

let createModel: (t, Uuid.t) => t
let deleteModel: (t, Uuid.t) => t
let focusModel: (t, Uuid.t) => t

let updateModel: (t, Uuid.t, UndoRedo.Action.t<Model.t>) => t
let updateModelState: (t, UndoRedo.Action.t<ModelState.t>) => t
let updateSlots: (t, Uuid.t, UndoRedo.Action.t<InspectorState.Schema.t>) => t

let renameModel: string => UndoRedo.Action.t<Model.t>
