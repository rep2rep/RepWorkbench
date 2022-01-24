module Model: {
  type t = {
    id: Uuid.t,
    name: string,
    model: ModelState.t,
    slots: Uuid.Map.t<InspectorState.Schema.t>,
  }
}

type t = {
  models: array<Model.t>,
  currentModel: option<int>,
}

let store: t => unit
let load: unit => option<t>
let empty: t

let name: t => option<string>
let modelState: t => ModelState.t
let inspectorState: t => InspectorState.t

let updateModel: (t, ModelState.t) => t
let updateSlots: (t, Uuid.t, option<InspectorState.Schema.t>) => t
